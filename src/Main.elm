module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra as ExDecode
import Json.Encode as Encode
import Task
import Time



-- MODEL -------------------------------------------------------------------


baseUrl =
    "http://localhost:11900"


type alias Username =
    String


type alias TweetBody =
    String


type alias UserInfo =
    { username : Username, password : String }


type alias Tweet =
    { id : String
    , author : Username
    , tweetBody : TweetBody
    , createdAt : Time.Posix
    }



{-
   Login時の情報は
      -- 見ているtimeline
      -- 自分のアカウント名(username)
      -- 送信しうるツイート本文(tweetBody)
      の3つから構成される
-}


type alias TripletWhenLogin =
    { timeline : List Tweet
    , username : Username
    , tweetBody : TweetBody
    }


type Model
    = Whoami
    | WithLoginHome TripletWhenLogin
    | GuestHome (List Tweet)
    | AccountSettings UserInfo
    | Failure Http.Error



-- まずログインしているかの確認を行う


init : () -> ( Model, Cmd Msg )
init _ =
    ( Whoami
    , riskyGet
        { url = baseUrl ++ "/whoami"
        , expect = Http.expectString GotMe
        }
    )


riskyGet : { url : String, expect : Http.Expect msg } -> Cmd msg
riskyGet { url, expect } =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


riskyPost : { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
riskyPost { url, body, expect } =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW --------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model of
        Whoami ->
            viewWhoami

        WithLoginHome triplet ->
            viewWithLoginHome triplet

        GuestHome timeline ->
            viewGuestHome timeline

        AccountSettings userinfo ->
            viewAccountSettings userinfo

        Failure error ->
            viewFailure error


viewWhoami : Html msg
viewWhoami =
    div [] [ text "Loading..." ]


viewWithLoginHome : TripletWhenLogin -> Html Msg
viewWithLoginHome { timeline, username, tweetBody } =
    div []
        [ div [] [ text ("Logging in as " ++ username) ]
        , div [] (viewTimeline timeline)
        , div []
            [ input [ type_ "text", value tweetBody, onInput FlushTweet ] []
            ]
        , div []
            [ button [ onClick ClickPostTweet ] [ text "Tweet" ]
            ]
        ]


viewGuestHome : List Tweet -> Html msg
viewGuestHome timeline =
    div []
        [ div [] [ text "Guest Home (Read Only)" ]
        , div [] (viewTimeline timeline)
        ]



-- sortのためにint型timeをkeyとする


makeInttimeTweetTuple : Tweet -> ( Int, Tweet )
makeInttimeTweetTuple tweet =
    ( Time.posixToMillis tweet.createdAt
    , tweet
    )


viewTweet : Tweet -> Html msg
viewTweet tweet =
    div [ class "tweet_container" ]
        [ div [] [ text tweet.author ]
        , div [] [ text tweet.tweetBody ]
        ]


viewTimeline : List Tweet -> List (Html msg)
viewTimeline timeline =
    List.map
        makeInttimeTweetTuple
        timeline
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> List.map viewTweet


viewAccountSettings : UserInfo -> Html Msg
viewAccountSettings userinfo =
    div []
        [ input [ type_ "text", value userinfo.username, onInput FlushUsername ] []
        , input [ type_ "password", value userinfo.password, onInput FlushPassword ] []
        , button [ onClick Login ] [ text "Login" ]
        , button [ onClick Register ] [ text "Register" ]
        ]


viewFailure : Http.Error -> Html msg
viewFailure error =
    case error of
        Http.BadUrl url ->
            div [] [ text ("Bad URL: " ++ url) ]

        Http.Timeout ->
            div [] [ text "Timeout" ]

        Http.NetworkError ->
            div [] [ text "Network error" ]

        Http.BadStatus code ->
            div [] [ text ("Bad request: " ++ String.fromInt code) ]

        Http.BadBody body ->
            div [] [ text ("BadBody: " ++ body) ]



-- UPDATE ------------------------------------------------------------------


type Msg
    = GotMe (Result Http.Error String)
    | GotTimeLine (Result Http.Error (List Tweet))
    | FlushTweet TweetBody
    | ClickPostTweet
    | GotTweeted (Result Http.Error String)
    | GotTweetTime Time.Posix
    | FlushUsername Username
    | FlushPassword String
    | Register
    | GotRegister (Result Http.Error String)
    | Login
    | GotLogin (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- ログインしているかの確認、403の場合に限りguest accountとしてtimelineを閲覧できるようになる
        ( GotMe result, Whoami ) ->
            case result of
                Ok username ->
                    ( WithLoginHome { timeline = [], username = username, tweetBody = "" }
                    , riskyGet
                        { url = baseUrl ++ "/timeline"
                        , expect = Http.expectJson GotTimeLine (Decode.list tweetDecoder)
                        }
                    )

                Err err ->
                    case err of
                        Http.BadStatus 403 ->
                            ( GuestHome []
                            , riskyGet
                                { url = baseUrl ++ "/tweet"
                                , expect = Http.expectJson GotTimeLine (Decode.list tweetDecoder)
                                }
                            )

                        otherwise ->
                            errorHandler err

        ( GotTimeLine result, WithLoginHome triplet ) ->
            case result of
                Ok timeline ->
                    ( WithLoginHome { triplet | timeline = timeline }
                    , Cmd.none
                    )

                Err err ->
                    errorHandler err

        ( GotTimeLine result, GuestHome _ ) ->
            case result of
                Ok timeline ->
                    ( GuestHome timeline
                    , Cmd.none
                    )

                Err err ->
                    errorHandler err

        ( FlushTweet tweetBody, WithLoginHome triplet ) ->
            ( WithLoginHome { triplet | tweetBody = tweetBody }, Cmd.none )

        -- tweet送信ボタンを押すと、まずその時間がcreatedAtとして記録される
        ( ClickPostTweet, WithLoginHome triplet ) ->
            ( model, Task.perform GotTweetTime Time.now )

        -- ツイート送信時間を取得すると即座にツイートを送信する
        ( GotTweetTime createdAt, WithLoginHome triplet ) ->
            let
                toBePostedTweet =
                    makeTweet triplet.tweetBody triplet.username createdAt
            in
            ( WithLoginHome
                { triplet
                    | timeline = toBePostedTweet :: triplet.timeline
                    , tweetBody = ""
                }
            , riskyPost
                { url = baseUrl ++ "/timeline"
                , body = Http.jsonBody <| tweetEncoder toBePostedTweet
                , expect = Http.expectString GotTweeted
                }
            )

        -- ツイートの送信に成功したかの確認
        ( GotTweeted result, WithLoginHome triplet ) ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err err ->
                    errorHandler err

        ( FlushUsername username, AccountSettings userinfo ) ->
            ( AccountSettings { userinfo | username = username }, Cmd.none )

        ( FlushPassword password, AccountSettings userinfo ) ->
            ( AccountSettings { userinfo | password = password }, Cmd.none )

        ( Register, AccountSettings userinfo ) ->
            ( model
            , Http.post
                { url = baseUrl ++ "/register"
                , body = Http.jsonBody <| userEncoder userinfo
                , expect = Http.expectString GotRegister
                }
            )

        ( GotRegister result, AccountSettings userinfo ) ->
            case result of
                Ok _ ->
                    ( model
                    , riskyPost
                        { url = baseUrl ++ "/login"
                        , body = Http.jsonBody <| userEncoder userinfo
                        , expect = Http.expectString GotLogin
                        }
                    )

                Err err ->
                    errorHandler err

        ( Login, AccountSettings userinfo ) ->
            ( model
            , riskyPost
                { url = baseUrl ++ "/login"
                , body = Http.jsonBody <| userEncoder userinfo
                , expect = Http.expectString GotLogin
                }
            )

        ( GotLogin result, AccountSettings userinfo ) ->
            case result of
                Ok _ ->
                    ( WithLoginHome { timeline = [], username = userinfo.username, tweetBody = "" }
                    , riskyGet
                        { url = baseUrl ++ "/timeline"
                        , expect = Http.expectJson GotTimeLine (Decode.list tweetDecoder)
                        }
                    )

                Err err ->
                    errorHandler err

        -- 上記以外の組み合わせは存在し得ない
        ( _, _ ) ->
            ( model, Cmd.none )


makeTweet : TweetBody -> Username -> Time.Posix -> Tweet
makeTweet tweetBody author createdAt =
    { id = "" -- id is generated by the server
    , tweetBody = tweetBody
    , author = author
    , createdAt = createdAt
    }


tweetEncoder : Tweet -> Encode.Value
tweetEncoder tweet =
    Encode.object
        [ ( "id", Encode.string tweet.id )
        , ( "tweetBody", Encode.string tweet.tweetBody )
        , ( "author", Encode.string tweet.author )
        , ( "created_at", Encode.string (Iso8601.fromTime tweet.createdAt) )
        ]


userEncoder : UserInfo -> Encode.Value
userEncoder userinfo =
    Encode.object
        [ ( "username", Encode.string userinfo.username )
        , ( "password", Encode.string userinfo.password )
        ]


tweetDecoder : Decoder Tweet
tweetDecoder =
    Decode.map4 Tweet
        (field "id" Decode.string)
        (field "tweet_body" Decode.string)
        (field "author" Decode.string)
        (field "created_at" ExDecode.datetime)


errorHandler : Http.Error -> ( Model, Cmd Msg )
errorHandler error =
    ( Failure error
    , Cmd.none
    )



-- MAIN --------------------------------------------------------------------


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- SUBSCRIPTIONS -----------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

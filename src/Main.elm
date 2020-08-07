module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra as ExDecode
import Json.Encode as Encode
import Time



-- MODEL -------------------------------------------------------------------


baseUrl =
    "http://localhost:11900"



-- twitterで言うところの@から始まる文字列


type alias UserID =
    String


type alias TweetBody =
    String


type alias UserInfo =
    { userID : UserID, password : String }


type alias PostTweetFormat =
    { userID : UserID
    , tweetBody : TweetBody
    }


type alias Tweet =
    { id : String
    , userID : UserID
    , tweetBody : TweetBody
    , createdAt : Time.Posix
    }



{-
   Login時の情報は
      -- 見ているtimeline
      -- 自分のアカウントID(userID)
      -- 送信しうるツイート本文(tweetBody)
      の3つから構成される
-}


type alias ElementWhenLogin =
    { timeline : List Tweet
    , userID : UserID
    , tweetBody : TweetBody
    }


type Model
    = Whoami
    | WithLoginHome ElementWhenLogin
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

        WithLoginHome loginElement ->
            viewWithLoginHome loginElement

        GuestHome timeline ->
            viewGuestHome timeline

        AccountSettings userinfo ->
            viewAccountSettings userinfo

        Failure error ->
            viewFailure error


viewWhoami : Html msg
viewWhoami =
    div [] [ text "Loading..." ]


viewWithLoginHome : ElementWhenLogin -> Html Msg
viewWithLoginHome { timeline, userID, tweetBody } =
    div []
        [ div [] [ text ("Logging in as " ++ userID) ]
        , div [] (viewTimeline timeline)
        , div []
            [ input [ type_ "text", value tweetBody, onInput FlushTweet ] []
            ]
        , div []
            [ button [ onClick PostTweet, disabled (tweetBody == "") ] [ text "Tweet" ]
            ]
        ]


viewGuestHome : List Tweet -> Html Msg
viewGuestHome timeline =
    div []
        [ div [] [ text "Guest Home (Read Only)" ]
        , button [ onClick GoAccountSettings ] [ text "Register or Login" ] -- TODO Browser.applicationにしたら消す
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
        [ div [] [ text tweet.userID ]
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
    let
        invalid =
            (String.length userinfo.userID <= 0) || (String.length userinfo.password <= 6)
    in
    div []
        [ input [ type_ "text", value userinfo.userID, onInput FlushUserID ] []
        , input [ type_ "password", value userinfo.password, onInput FlushPassword ] []
        , button [ onClick Login, disabled invalid ] [ text "Login" ]
        , button [ onClick Register, disabled invalid ] [ text "Register" ]
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
    | GoAccountSettings
    | GotTimeLine (Result Http.Error (List Tweet))
    | FlushTweet TweetBody
    | PostTweet
    | GotTweeted (Result Http.Error Tweet)
    | FlushUserID UserID
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
                Ok userID ->
                    ( WithLoginHome { timeline = [], userID = userID, tweetBody = "" }
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

        ( GotTimeLine result, WithLoginHome loginElement ) ->
            case result of
                Ok timeline ->
                    ( WithLoginHome { loginElement | timeline = timeline }
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

        ( GoAccountSettings, GuestHome _ ) ->
            ( AccountSettings { userID = "", password = "" }, Cmd.none )

        ( FlushTweet tweetBody, WithLoginHome loginElement ) ->
            ( WithLoginHome { loginElement | tweetBody = tweetBody }, Cmd.none )

        ( PostTweet, WithLoginHome loginElement ) ->
            ( model
            , riskyPost
                { url = baseUrl ++ "/tweet"
                , body =
                    Http.jsonBody <|
                        tweetEncoder
                            { userID = loginElement.userID
                            , tweetBody = loginElement.tweetBody
                            }
                , expect = Http.expectJson GotTweeted tweetDecoder
                }
            )

        -- ツイートの送信に成功したかの確認
        ( GotTweeted result, WithLoginHome loginElement ) ->
            case result of
                Ok postedTweet ->
                    ( WithLoginHome
                        { loginElement
                            | timeline = postedTweet :: loginElement.timeline
                            , tweetBody = ""
                        }
                    , Cmd.none
                    )

                Err err ->
                    errorHandler err

        ( FlushUserID userID, AccountSettings userinfo ) ->
            ( AccountSettings { userinfo | userID = userID }, Cmd.none )

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
                    ( WithLoginHome { timeline = [], userID = userinfo.userID, tweetBody = "" }
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


tweetEncoder : PostTweetFormat -> Encode.Value
tweetEncoder tweet =
    Encode.object
        [ ( "user_id", Encode.string tweet.userID )
        , ( "tweet_body", Encode.string tweet.tweetBody )
        ]


userEncoder : UserInfo -> Encode.Value
userEncoder userinfo =
    Encode.object
        [ ( "user_id", Encode.string userinfo.userID )
        , ( "password", Encode.string userinfo.password )
        ]


tweetDecoder : Decoder Tweet
tweetDecoder =
    Decode.map4 Tweet
        (field "id" Decode.string)
        (field "user_id" Decode.string)
        (field "tweet_body" Decode.string)
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

module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, form, h1, input, label, li, text, ul)
import Html.Attributes exposing (class, for, id, method, required, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field)
import Json.Encode as Encode
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL --------------------------------------------------------------------------


baseUrl =
    "http://localhost:11900"


type alias UserName =
    String


type Model
    = Whoami
    | GuestHome
    | WithLoginHome UserName
    | Account UserInfo


init : () -> ( Model, Cmd Msg )
init _ =
    ( Whoami
    , riskyGet
        { url = baseUrl ++ "/users/me"
        , expect = Http.expectJSON GotMe meDecoder
        }
    )


riskyGet : { url : String, expect : Expect msg } -> Cmd msg
riskyGet { url, expect } =
    Http.riskyRequest
        { method = "GET"
        , header = []
        , url = url
        , body = emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


riskyPost : { url : String, expect : Expect msg } -> Cmd msg
riskyPost { url, expect } =
    Http.riskyRequest
        { method = "POST"
        , header = []
        , url = url
        , body = emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW ---------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model of
        Whoami ->
            viewWhoami

        IsLoggedIn ->
            viewLogin

        GuestHome ->
            viewGuestHome

        WithLoginHome timeline ->
            viewLoginHome timeline

        Account userinfo ->
            viewAccountSettings userinfo



-- UPDATE -------------------------------------------------------------------------


type Msg
    = GotMe (Result Http.Error String)
    | GotTimeLine (Result Http.Error (List JsonTweet))
    | FlushUsername String
    | FlushPassword String
    | SignUp
    | SignIn
    | GotSignUp (Result Http.Error ()) -- TODO : String for error handle
    | GotSignIn (Result Http.Error ())
    | PostTweet (Result Http.Error String)
    | TweetTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotMe result, _ ) ->
            case result of
                Ok username ->
                    ( WithLoginHome userinfo []
                    , getTimeLine
                    )

                Err err ->
                    errorHandler err

        ( GotTimeLine result, WithLoginHome userinfo _ ) ->
            case result of
                Ok timeline ->
                    ( WithLoginHome userinfo timeline
                    , Cmd.none
                    )

                Err err ->
                    portalErrorHandler err userinfo

        ( FlushUsername username, Portal message userinfo ) ->
            ( Portal message { userinfo | username = username }, Cmd.none )

        ( FlushPassword password, Portal message userinfo ) ->
            ( Portal message { userinfo | password = password }, Cmd.none )

        ( SignUp, Portal _ userinfo ) ->
            ( Portal "Register..." userinfo
            , Http.post
                { url = baseUrl ++ "/register"
                , body = Http.jsonBody <| userToValue userinfo
                , expect = Http.expectWhatever GotSignUp
                }
            )

        ( GotSignUp result, Portal message userinfo ) ->
            case result of
                Ok _ ->
                    ( Portal message userinfo
                    , postSignIn userinfo
                    )

                Err err ->
                    portalErrorHandler err userinfo

        ( SignIn, Portal _ userinfo ) ->
            ( Portal "Login..." userinfo
            , postSignIn userinfo
            )

        ( GotSignIn result, Portal message userinfo ) ->
            case result of
                Ok _ ->
                    ( CityPage (CityInfo 0 "" "" "" 0), Cmd.none )

                Err err ->
                    portalErrorHandler err userinfo

        ( ClickPost, WithLoginHome userinfo ) ->
            ( WithLoginHome userinfo
            , Task.Perform TweetTime Time.now
            )

        ( TweetTime now, WithLoginHome userinfo ) ->
            ( WithLoginHome userinfo
            , riskyPost
                { url = baseUrl ++ "/tweet"
                , expect = Http.expectString GotResponse
                }
            )

        ( GotResponse result, _ ) ->
            ( WithLoginHome userinfo
            , httpErrorHandler result
            )

        ( _, _ ) ->
            ( model, Cmd.none )


postSignIn : UserInfo -> Cmd Msg
postSignIn userinfo =
    Http.post
        { url = baseUrl ++ "/login"
        , body = Http.jsonBody <| userToValue userinfo
        , expect = Http.expectWhatever GotSignIn
        }


userToValue : UserInfo -> Encode.Value
userToValue userinfo =
    Encode.object
        [ ( "username", Encode.string userinfo.username )
        , ( "password", Encode.string userinfo.password )
        ]


portalErrorHandler : Http.Error -> UserInfo -> ( Model, Cmd Msg )
portalErrorHandler err userinfo =
    case err of
        Http.BadStatus code ->
            ( Portal ("Your request is reject because of " ++ String.fromInt code) userinfo
            , Cmd.none
            )

        Http.BadUrl url ->
            ( Portal ("Wrong URL: " ++ url) userinfo, Cmd.none )

        Http.Timeout ->
            ( Portal "Timeout" userinfo, Cmd.none )

        Http.NetworkError ->
            ( Portal "Network Error" userinfo, Cmd.none )

        Http.BadBody body ->
            ( Portal ("Your request body is wrong: " ++ body) userinfo, Cmd.none )


tweetDecoder : Decoder JsonTweet
tweetDecoder =
    Decode.map4 JsonTweet
        (field "uuid" Decode.string)
        (field "message" Decode.string)
        (field "username" Decode.string)
        (field "created_at" Decode.int)


rawToPosix : JsonRawTweet -> JsonTweet
rawToPosix raw =
    { raw | created_at = Time.millisToPosix raw.created_at }


posixToRaw : JsonTweet -> JsonRawTweet
posixToRaw tweet =
    { tweet | created_at = Time.posixToMillis raw.created_at }


type alias JsonRawTweet =
    { uuid : String -- 空の文字列を渡してサーバで作ってもらう
    , message : String
    , username : String -- ツイートした人
    , created_at : Int
    }


type alias JsonTweet =
    { uuid : String -- 空の文字列を渡してサーバで作ってもらう
    , message : String
    , username : String -- ツイートした人
    , created_at : Time.Posix
    }


type alias UserInfo =
    { username : String
    , password : String
    }

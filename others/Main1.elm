module Main1 exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode


type alias Model =
    { position : Position }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { position =
            { x = 0
            , y = 0
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragAt position ->
            ( { model | position = position }, Cmd.none )


type Msg
    = DragAt Position


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onMouseMove <| Decode.map DragAt positionDecoder


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


type alias Position =
    { x : Int
    , y : Int
    }


view : Model -> Html Msg
view model =
    layout
        [ inFront <|
            el
                [ Background.color <| rgb 1 1 0.8
                , width <| px 300
                , height <| px 300
                , padding 20
                , Border.width 4
                , Border.color <| rgb 0.8 0.8 0.6
                , moveRight 300
                , moveDown 300
                ]
            <|
                text "ciao"
        ]
    <|
        row [ spacing 20 ]
            [ text <| String.fromInt model.position.x
            , text <| String.fromInt model.position.y
            ]

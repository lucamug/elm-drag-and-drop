module Main4 exposing (main)

import Browser
import Draggable
import Draggable.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes


type alias Position =
    { x : Float
    , y : Float
    }


type alias Model =
    { xy : Position
    , drag : Draggable.State ()
    , isDragging : Bool
    , isClicked : Bool
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { xy = Position 32 32
      , drag = Draggable.init
      , isDragging = False
      , isClicked = False
      }
    , Cmd.none
    )


type Msg
    = OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg ())
    | OnDragStart
    | OnDragEnd


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- dragConfig : Draggable.Config () Msg
-- dragConfig =
--     Draggable.basicConfig OnDragBy


dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragStart (\_ -> OnDragStart)
        , Draggable.Events.onDragEnd OnDragEnd
        , Draggable.Events.onDragBy OnDragBy

        --, onClick (\_ -> OnClick)
        --, Draggable.Events.onMouseDown (\_ -> SetClicked True)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ xy } as model) =
    case msg of
        OnDragBy ( dx, dy ) ->
            ( { model | xy = Position (xy.x + dx) (xy.y + dy) }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        OnDragStart ->
            ( { model | isDragging = True }, Cmd.none )

        OnDragEnd ->
            ( { model | isDragging = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


view : Model -> Html Msg
view model =
    let
        translate =
            "translate(" ++ String.fromFloat model.xy.x ++ "px, " ++ String.fromFloat model.xy.y ++ "px)"
    in
    layout
        [ inFront <|
            el
                ([ htmlAttribute <| Html.Attributes.style "cursor" "move"
                 , htmlAttribute <| Draggable.mouseTrigger () DragMsg
                 , htmlAttribute <| Html.Attributes.style "transform" translate
                 , Border.width 10
                 , Background.color <| rgb 0 0.6 0.8
                 , padding 30
                 ]
                    ++ List.map (\x -> htmlAttribute x) (Draggable.touchTriggers () DragMsg)
                )
            <|
                text "ciao"
        ]
    <|
        column []
            [ text <| Debug.toString model
            , text "ciao"
            ]



{-
   Html.div
       ([ Html.Attributes.style "transform" translate
        , Html.Attributes.style "padding" "16px"
        , Html.Attributes.style "background-color" "lightgray"
        , Html.Attributes.style "width" "64px"
        , Html.Attributes.style "cursor" "move"
        , Draggable.mouseTrigger () DragMsg
        ]
           ++ Draggable.touchTriggers () DragMsg
       )
       [ Html.text "Drag me" ]
-}

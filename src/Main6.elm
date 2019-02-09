module Main6 exposing (main)

import Browser
import Draggable
import Draggable.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { x : Float
    , y : Float
    }


type alias ID =
    String


type alias Component =
    { id : ID
    , position : Position
    }


type alias Area =
    { id : ID
    , position : Position
    , size : Size
    }


type alias Model =
    { components : List Component
    , areas : List Area
    , isDragging : Bool
    , isMouseDown : ID
    , areaIdWithMouseOver : Maybe ID
    , drag : Draggable.State String
    }


type Msg
    = OnDragBy Draggable.Delta
    | OnDragStart ID
    | OnDragEnd
    | OnClick ID
    | OnMouseDown ID
    | DragMsg (Draggable.Msg String)
    | MouseEnterArea ID
    | MouseLeaveArea ID


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { components =
            [ { id = "01", position = Position 0 0 }
            , { id = "02", position = Position 0 0 }
            , { id = "03", position = Position 0 0 }
            , { id = "05", position = Position 0 0 }
            ]
      , areas =
            [ { id = "Area 1", position = Position 0 0, size = Size 100 200 }
            , { id = "Area 2", position = Position 100 0, size = Size 100 100 }
            , { id = "Area 3", position = Position 200 0, size = Size 300 100 }
            , { id = "Area 4", position = Position 100 100, size = Size 200 200 }
            , { id = "Area 5", position = Position 0 200, size = Size 100 100 }
            , { id = "Area 6", position = Position 300 100, size = Size 100 200 }
            , { id = "Area 7", position = Position 400 100, size = Size 100 100 }
            , { id = "Area 8", position = Position 400 200, size = Size 100 100 }
            , { id = "Area 9", position = Position 0 300, size = Size 500 100 }
            , { id = "Area 10", position = Position 0 400, size = Size 400 100 }
            , { id = "Area 11", position = Position 400 400, size = Size 100 100 }
            ]
      , drag = Draggable.init
      , isDragging = False
      , isMouseDown = ""
      , areaIdWithMouseOver = Nothing
      }
    , Cmd.none
    )


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragStart OnDragStart
        , Draggable.Events.onDragEnd OnDragEnd
        , Draggable.Events.onDragBy OnDragBy
        , Draggable.Events.onClick OnClick
        , Draggable.Events.onMouseDown OnMouseDown
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseEnterArea id ->
            ( { model | areaIdWithMouseOver = Just id }, Cmd.none )

        MouseLeaveArea id ->
            ( { model | areaIdWithMouseOver = Nothing }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                components =
                    List.map
                        (\component ->
                            if component.id == model.isMouseDown then
                                { component
                                    | position =
                                        { x = component.position.x + dx
                                        , y = component.position.y + dy
                                        }
                                }

                            else
                                component
                        )
                        model.components
            in
            ( { model | components = components }
            , Cmd.none
            )

        OnDragStart id ->
            ( { model | isDragging = True }, Cmd.none )

        OnDragEnd ->
            let
                -- Put the component in the initial position, for the moment
                components =
                    List.map
                        (\component ->
                            if component.id == model.isMouseDown then
                                { component
                                    | position =
                                        { x = 0
                                        , y = 0
                                        }
                                }

                            else
                                component
                        )
                        model.components
            in
            ( { model | isDragging = False, components = components }, Cmd.none )

        OnClick id ->
            ( model, Cmd.none )

        OnMouseDown id ->
            ( { model | isMouseDown = id }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


componentDragging : Model -> ID -> Bool
componentDragging model id =
    model.isMouseDown == id && model.isDragging


viewItem : Model -> ID -> Element Msg
viewItem model id =
    let
        temp =
            List.head (List.filter (\component -> component.id == id) model.components)

        translate =
            case temp of
                Just component ->
                    "translate(" ++ String.fromFloat component.position.x ++ "px, " ++ String.fromFloat component.position.y ++ "px) scale(1.3, 1.3)"

                Nothing ->
                    -- This should never happen
                    ""

        color =
            if componentDragging model id then
                "limegreen"

            else
                "lightgray"
    in
    column
        [ htmlAttribute <| Html.Attributes.style "transform" translate
        , htmlAttribute <| Html.Attributes.style "transition" "transform 0.05s, opacity 0.3s ease"
        , htmlAttribute <|
            Html.Attributes.style "pointer-events"
                -- This is to detect mouseOver event also when
                -- the mouse is draggin stuff
                (if componentDragging model id then
                    "none"

                 else
                    "auto"
                )
        , padding 16
        , alpha <|
            if componentDragging model id then
                0.8

            else
                1
        , htmlAttribute <|
            Html.Attributes.style "z-index" <|
                if componentDragging model id then
                    "1"

                else
                    "auto"
        , Background.color <| rgb 0 0.8 0.5
        , Border.width 10
        , Border.color <|
            if componentDragging model id then
                rgb 0 0.5 0.3

            else
                rgba 0 0 0 0
        , width <| px 200
        , height <| px 80
        , htmlAttribute <| Html.Attributes.style "cursor" "move"
        , htmlAttribute <| Draggable.mouseTrigger id DragMsg
        , htmlAttribute <| Html.Events.onMouseUp <| OnMouseDown id
        ]
        [ text id
        ]


viewArea : Model -> Area -> Attribute Msg
viewArea model area =
    inFront <|
        el
            [ width <| px <| round area.size.x
            , height <| px <| round area.size.y
            , Background.color <|
                if model.areaIdWithMouseOver == Just area.id then
                    rgb 0.8 0.2 0.2

                else
                    rgb 0.8 0.8 0.8
            , moveRight area.position.x
            , moveDown area.position.y
            , Border.width 5
            , Border.color <| rgb 0.9 0.9 0.9
            , htmlAttribute <| Html.Events.onMouseEnter <| MouseEnterArea area.id
            , htmlAttribute <| Html.Events.onMouseLeave <| MouseLeaveArea area.id
            ]
        <|
            el
                [ centerX
                , centerY
                ]
            <|
                text area.id


view : Model -> Html Msg
view model =
    layout
        [ padding 10 ]
    <|
        column []
            [ column
                [ spacing 5
                , paddingXY 0 10
                ]
                [ el [] <| text <| "isDragging: " ++ Debug.toString model.isDragging
                , el [] <| text <| "isMouseDown: " ++ Debug.toString model.isMouseDown
                , el [] <| text <| "drag: " ++ Debug.toString model.drag
                , el [] <| text <| "components: " ++ Debug.toString model.components
                , el [] <| text <| "areaIdWithMouseOver: " ++ Debug.toString model.areaIdWithMouseOver
                , link [] { label = text "Packery", url = "https://packery.metafizzy.co/" }
                ]
            , row [ width fill ]
                [ column
                    [ paddingXY 40 20
                    , spacing 40
                    , htmlAttribute <| Html.Attributes.style "z-index" "1"
                    , Background.color <| rgb 0.8 1 0.8
                    , height fill
                    ]
                  <|
                    List.map (\component -> viewItem model component.id) model.components
                , el
                    [ padding 5
                    , Background.color <| rgb 0.9 0.9 0.9
                    ]
                  <|
                    column
                        ([ Background.color <| rgb 0.9 0.9 0.9
                         , width <| px 500
                         , height <| px 500
                         ]
                            ++ List.map (\area -> viewArea model area) model.areas
                        )
                        [ none ]
                ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

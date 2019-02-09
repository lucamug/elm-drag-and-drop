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


type alias MenuItem =
    { id : ID
    , position : Position
    }


type alias Model =
    { xy : Position
    , positions : List MenuItem
    , isClick : Maybe ID
    , isDragStart : Maybe ID
    , drag : Draggable.State String

    --
    , isClicked : Bool

    --
    , isMouseUp : Maybe ID
    , isMouseDown : Maybe ID
    , isMouseEnter : Maybe ID
    , isMouseLeave : Maybe ID
    }


type alias ID =
    String


type Msg
    = OnDragBy Draggable.Delta
    | OnDragStart ID
    | OnDragEnd
    | OnClick ID
    | DragMsg (Draggable.Msg ID)
      --
    | SetClicked Bool
      --
    | OnMouseUp ID
    | OnMouseDown ID
    | OnMouseEnter ID
    | OnMouseLeave ID


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { xy = Position 32 32
      , positions =
            [ { id = "01", position = Position 32 32 }
            , { id = "02", position = Position 32 32 }
            ]
      , drag = Draggable.init
      , isClick = Nothing
      , isDragStart = Nothing

      --
      , isClicked = False

      --
      , isMouseUp = Nothing
      , isMouseDown = Nothing
      , isMouseEnter = Nothing
      , isMouseLeave = Nothing
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
        , Draggable.Events.onMouseDown (\_ -> SetClicked True)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        xy =
            model.xy
    in
    case msg |> Debug.log "" of
        OnDragBy ( dx, dy ) ->
            ( { model | xy = Position (xy.x + dx) (xy.y + dy) }
            , Cmd.none
            )

        OnDragStart id ->
            ( { model | isDragStart = Just id }, Cmd.none )

        OnDragEnd ->
            ( { model | isDragStart = Nothing }, Cmd.none )

        OnClick id ->
            ( { model | isClick = Just id }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        --
        SetClicked flag ->
            ( { model | isClicked = flag }, Cmd.none )

        --
        OnMouseUp id ->
            ( { model | isMouseUp = Just id }, Cmd.none )

        OnMouseDown id ->
            ( { model | isMouseDown = Just id }, Cmd.none )

        OnMouseEnter id ->
            ( { model | isMouseEnter = Just id }, Cmd.none )

        OnMouseLeave id ->
            ( { model | isMouseLeave = Just id }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


view : Model -> Html Msg
view model =
    layout
        []
    <|
        column []
            [ column [ spacing 10 ]
                [ el [] <| text <| "isClick: " ++ Debug.toString model.isClick
                , el [] <| text <| "isDragStart: " ++ Debug.toString model.isDragStart

                --
                , el [] <| text <| "isMouseUp: " ++ Debug.toString model.isMouseUp
                , el [] <| text <| "isMouseDown: " ++ Debug.toString model.isMouseDown
                , el [] <| text <| "isMouseEnter: " ++ Debug.toString model.isMouseEnter
                , el [] <| text <| "isMouseLeave: " ++ Debug.toString model.isMouseLeave
                ]
            , paragraph []
                [ el [] <| text <| Debug.toString model
                ]
            , viewItem model "01"
            , viewItem model "02"
            ]


viewItem : Model -> ID -> Element Msg
viewItem model id =
    let
        translate =
            if model.isMouseDown == Just id then
                "translate(" ++ String.fromFloat model.xy.x ++ "px, " ++ String.fromFloat model.xy.y ++ "px) scale(1.3, 1.3)"

            else
                "translate(0px, 0px) scale(1,1)"

        status =
            if model.isDragStart == Just id then
                "Release me"

            else
                "Drag me"

        color =
            if model.isMouseDown == Just id then
                "limegreen"

            else
                "lightgray"
    in
    column
        [ htmlAttribute <| Html.Attributes.style "transform" translate
        , htmlAttribute <| Html.Attributes.style "transition" "transform 0.05s, opacity 0.3s ease 0.5s"

        --, htmlAttribute <| Html.Attributes.style "transition" "height 0.3s ease-out"
        , padding 16
        , Background.color <| rgb 0 0.8 0.5
        , Border.width 10
        , Border.color <|
            if model.isMouseDown == Just id then
                rgb 0 0.5 0.3

            else
                rgba 1 1 1 1
        , width <| px 200
        , height <| px 80
        , htmlAttribute <| Html.Attributes.style "cursor" "move"
        , htmlAttribute <| Draggable.mouseTrigger id DragMsg
        , htmlAttribute <| Html.Events.onMouseUp <| OnMouseUp id
        , htmlAttribute <| Html.Events.onMouseDown <| OnMouseDown id
        , htmlAttribute <| Html.Events.onMouseEnter <| OnMouseEnter id
        , htmlAttribute <| Html.Events.onMouseLeave <| OnMouseLeave id

        --
        , htmlAttribute <| Html.Events.onMouseUp (SetClicked False)
        ]
        [ text id
        ]


css : String
css =
    """
"""

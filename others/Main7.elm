module Main exposing (main)

import Browser
import Draggable
import Draggable.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events


sizeDefault : Int
sizeDefault =
    100


npos : Int -> Float
npos pos =
    toFloat <| pos * sizeDefault


dimensionDefault : Size
dimensionDefault =
    Size 5 5


borderWidthDefault : Int
borderWidthDefault =
    5


labelEmpty : String
labelEmpty =
    "empty"


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { x : Int
    , y : Int
    }


type alias ID =
    String


type alias Component =
    { id : ID
    , position : Position
    , size : Size
    , content : Maybe (Element Msg)
    }


emptyContent : Element msg
emptyContent =
    text "☆"


maybeEmptyContent : Maybe (Element msg)
maybeEmptyContent =
    Just emptyContent


emptyComponent : Component
emptyComponent =
    { id = "", position = Position (npos 0) (npos 0), size = Size 0 0, content = maybeEmptyContent }


type alias Model =
    { componentsInMenu : List Component
    , componentsInDashboard : List Component
    , maybeDraggedComponent : Maybe { id : ID, initialPosition : Position }
    , isMouseUp : ID
    , idComponentWithMouseOver : Maybe ID
    , drag : Draggable.State String
    , dragStartPosition : Maybe Position
    }


type Msg
    = OnDragBy Draggable.Delta
    | OnDragStart ID
    | OnDragEnd
    | OnClick ID
    | OnMouseDown ID
    | DragMsg (Draggable.Msg String)
    | MouseEnterComponent ID
    | MouseLeaveComponent ID
    | RemoveComponent ID


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { componentsInMenu =
            [ { id = "Comp. 4", position = Position (npos 0) (npos 0), size = Size 2 1, content = maybeEmptyContent }
            , { id = "Comp. 3", position = Position (npos 0) (npos 0), size = Size 2 2, content = maybeEmptyContent }

            --, { id = "Comp. 1", position = Position (npos 0) (npos 0), size = Size 1 1, content = maybeEmptyContent }
            --, { id = "Comp. 2", position = Position (npos 0) (npos 0), size = Size 1 2, content = maybeEmptyContent }
            ]
      , componentsInDashboard =
            [ { id = "Component 1", position = Position (npos 0) (npos 0), size = Size 1 2, content = maybeEmptyContent }
            , { id = "Component 2", position = Position (npos 1) (npos 0), size = Size 1 1, content = maybeEmptyContent }
            , { id = "Component 3", position = Position (npos 2) (npos 0), size = Size 3 1, content = maybeEmptyContent }

            --, { id = "Component 4", position = Position (npos 1) (npos 1), size = Size 2 2, content = maybeEmptyContent }
            , { id = "Component 5", position = Position (npos 0) (npos 2), size = Size 1 1, content = maybeEmptyContent }

            --, { id = "Component 6", position = Position (npos 3) (npos 1), size = Size 1 2, content = maybeEmptyContent }
            , { id = "Component 7", position = Position (npos 4) (npos 1), size = Size 1 1, content = maybeEmptyContent }
            , { id = "Component 8", position = Position (npos 4) (npos 2), size = Size 1 1, content = maybeEmptyContent }

            --, { id = "Component 9", position = Position (npos 0) (npos 3), size = Size 5 1, content = maybeEmptyContent }
            , { id = "Component 10", position = Position (npos 0) (npos 4), size = Size 4 1, content = maybeEmptyContent }
            , { id = "Component 11", position = Position (npos 4) (npos 4), size = Size 1 1, content = maybeEmptyContent }
            ]
      , drag = Draggable.init
      , maybeDraggedComponent = Nothing
      , isMouseUp = ""
      , idComponentWithMouseOver = Nothing
      , dragStartPosition = Nothing
      }
    , Cmd.none
    )


positionToId : String -> Position -> ID
positionToId label { x, y } =
    label ++ "_" ++ String.fromFloat x ++ "_" ++ String.fromFloat y


idToPosition : String -> ID -> Maybe Position
idToPosition label id =
    let
        list =
            String.split "_" id
    in
    case list of
        [] ->
            Nothing

        [ _ ] ->
            Nothing

        [ _, _ ] ->
            Nothing

        label_ :: x :: y :: _ ->
            if label_ == label then
                case String.toFloat x of
                    Just x_ ->
                        case String.toFloat y of
                            Just y_ ->
                                Just <| Position x_ y_

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            else
                Nothing


componentsForEmpyAreas : Model -> List Component
componentsForEmpyAreas model =
    {- TODO
       This function should cover all empty areas automatically
    -}
    [ { id = positionToId labelEmpty <| Position (npos 1) (npos 1), position = Position (npos 1) (npos 1), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 2) (npos 1), position = Position (npos 2) (npos 1), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 3) (npos 1), position = Position (npos 3) (npos 1), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 1) (npos 2), position = Position (npos 1) (npos 2), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 2) (npos 2), position = Position (npos 2) (npos 2), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 3) (npos 2), position = Position (npos 3) (npos 2), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 0) (npos 3), position = Position (npos 0) (npos 3), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 1) (npos 3), position = Position (npos 1) (npos 3), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 2) (npos 3), position = Position (npos 2) (npos 3), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 3) (npos 3), position = Position (npos 3) (npos 3), size = Size 1 1, content = Nothing }
    , { id = positionToId labelEmpty <| Position (npos 4) (npos 3), position = Position (npos 4) (npos 3), size = Size 1 1, content = Nothing }
    ]


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragStart OnDragStart
        , Draggable.Events.onDragEnd OnDragEnd
        , Draggable.Events.onDragBy OnDragBy
        , Draggable.Events.onClick OnClick
        , Draggable.Events.onMouseDown OnMouseDown
        ]


dropComponent : Model -> List Component -> List Component
dropComponent model components =
    case getOverlay model of
        Just overlay ->
            overlay :: components

        Nothing ->
            components


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoveComponent id ->
            let
                componentsInDashboard =
                    List.filter (\component -> component.id /= id) model.componentsInDashboard
            in
            ( { model | componentsInDashboard = componentsInDashboard }, Cmd.none )

        MouseEnterComponent id ->
            ( { model | idComponentWithMouseOver = Just id }, Cmd.none )

        MouseLeaveComponent id ->
            ( { model | idComponentWithMouseOver = Nothing }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                componentsInMenu =
                    case model.maybeDraggedComponent of
                        Just draggingComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggingComponent.id then
                                        { component
                                            | position =
                                                { x = component.position.x + dx
                                                , y = component.position.y + dy
                                                }
                                        }

                                    else
                                        component
                                )
                                model.componentsInMenu

                        Nothing ->
                            model.componentsInMenu

                componentsInDashboard =
                    case model.maybeDraggedComponent of
                        Just draggingComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggingComponent.id then
                                        { component
                                            | position =
                                                { x = component.position.x + dx
                                                , y = component.position.y + dy
                                                }
                                        }

                                    else
                                        component
                                )
                                model.componentsInDashboard

                        Nothing ->
                            model.componentsInDashboard
            in
            ( { model
                | componentsInMenu = componentsInMenu
                , componentsInDashboard = componentsInDashboard
              }
            , Cmd.none
            )

        OnDragStart id ->
            let
                _ =
                    Debug.log "OnDragStart" id

                component =
                    getComponent (model.componentsInDashboard ++ model.componentsInMenu) id
            in
            ( { model
                | maybeDraggedComponent = Just { id = id, initialPosition = component.position }
              }
            , Cmd.none
            )

        OnDragEnd ->
            let
                _ =
                    Debug.log "OnDragEnd" model.maybeDraggedComponent

                -- Put the component in the initial position, for the moment.
                componentsInMenu =
                    case model.maybeDraggedComponent of
                        Just draggingComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggingComponent.id then
                                        { component
                                            | position =
                                                Position draggingComponent.initialPosition.x
                                                    draggingComponent.initialPosition.y
                                        }

                                    else
                                        component
                                )
                                model.componentsInMenu

                        Nothing ->
                            model.componentsInMenu

                componentsInDashboard =
                    case model.maybeDraggedComponent of
                        Just draggingComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggingComponent.id then
                                        { component
                                            | position =
                                                Position draggingComponent.initialPosition.x
                                                    draggingComponent.initialPosition.y
                                        }

                                    else
                                        component
                                )
                                model.componentsInDashboard

                        Nothing ->
                            model.componentsInDashboard

                componentsInDashboard2 =
                    dropComponent model componentsInDashboard
            in
            ( { model
                | maybeDraggedComponent = Nothing
                , componentsInMenu = componentsInMenu
                , componentsInDashboard = componentsInDashboard2
              }
            , Cmd.none
            )

        OnClick id ->
            ( { model | maybeDraggedComponent = Nothing }, Cmd.none )

        OnMouseDown id ->
            {-
               let
                   _ =
                       Debug.log "OnMouseDown" id

                   component =
                       getComponent (model.componentsInDashboard ++ model.componentsInMenu) id
               in
               ( { model
                   | maybeDraggedComponent = Just { id = id, initialPosition = component.position }
                 }
               , Cmd.none
               )
            -}
            ( model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


componentDragging : Model -> ID -> Bool
componentDragging model id =
    case model.maybeDraggedComponent of
        Just draggingComponent ->
            draggingComponent.id == id

        Nothing ->
            False


getComponent : List Component -> ID -> Component
getComponent components id =
    let
        temp =
            List.head (List.filter (\component_ -> component_.id == id) components)
    in
    case temp of
        Just component_ ->
            component_

        Nothing ->
            emptyComponent


getOverlay : Model -> Maybe Component
getOverlay model =
    {- TODO
       This function should detect all available positions based on dragged item.
    -}
    if isDragging model then
        let
            component =
                getComponent model.componentsInMenu <| .id <| Maybe.withDefault { id = "", initialPosition = Position 0 0 } model.maybeDraggedComponent

            id x y =
                Just (positionToId labelEmpty (Position (npos x) (npos y)))

            empty x y dx dy =
                Just { id = "Overlay", position = Position (npos x) (npos y), size = Size dx dy, content = Nothing }
        in
        if component.size == Size 2 1 then
            if model.idComponentWithMouseOver == id 1 1 then
                empty 1 1 2 1

            else if model.idComponentWithMouseOver == id 2 1 || model.idComponentWithMouseOver == id 3 1 then
                empty 2 1 2 1

            else if model.idComponentWithMouseOver == id 1 2 then
                empty 1 2 2 1

            else if model.idComponentWithMouseOver == id 2 2 || model.idComponentWithMouseOver == id 3 2 then
                empty 2 2 2 1

            else if model.idComponentWithMouseOver == id 0 3 then
                empty 0 3 2 1

            else if model.idComponentWithMouseOver == id 1 3 then
                empty 1 3 2 1

            else if model.idComponentWithMouseOver == id 2 3 then
                empty 2 3 2 1

            else if model.idComponentWithMouseOver == id 3 3 || model.idComponentWithMouseOver == id 4 3 then
                empty 3 3 2 1

            else
                Nothing

        else if component.size == Size 2 2 then
            if
                model.idComponentWithMouseOver
                    == id 1 1
                    || model.idComponentWithMouseOver
                    == id 2 1
                    || model.idComponentWithMouseOver
                    == id 2 2
                    || model.idComponentWithMouseOver
                    == id 1 2
            then
                empty 1 1 2 2

            else if
                model.idComponentWithMouseOver
                    == id 3 1
                    || model.idComponentWithMouseOver
                    == id 3 2
            then
                empty 2 1 2 2

            else if
                model.idComponentWithMouseOver
                    == id 1 3
                    || model.idComponentWithMouseOver
                    == id 2 3
            then
                empty 1 2 2 2

            else if
                model.idComponentWithMouseOver
                    == id 3 3
            then
                empty 2 2 2 2

            else
                Nothing

        else
            Nothing

    else
        Nothing



{-
   ██    ██ ██ ███████ ██     ██      ██████  ██████  ███    ███ ██████   ██████  ███    ██ ███████ ███    ██ ████████ ███████
   ██    ██ ██ ██      ██     ██     ██      ██    ██ ████  ████ ██   ██ ██    ██ ████   ██ ██      ████   ██    ██    ██
   ██    ██ ██ █████   ██  █  ██     ██      ██    ██ ██ ████ ██ ██████  ██    ██ ██ ██  ██ █████   ██ ██  ██    ██    ███████
    ██  ██  ██ ██      ██ ███ ██     ██      ██    ██ ██  ██  ██ ██      ██    ██ ██  ██ ██ ██      ██  ██ ██    ██         ██
     ████   ██ ███████  ███ ███       ██████  ██████  ██      ██ ██       ██████  ██   ████ ███████ ██   ████    ██    ███████
-}


attrsSize : Component -> List (Attribute Msg)
attrsSize component =
    [ width <| px <| component.size.x * sizeDefault
    , height <| px <| component.size.y * sizeDefault
    , Border.width borderWidthDefault
    ]


attrsPosition : Component -> List (Attribute Msg)
attrsPosition component =
    [ moveRight <| component.position.x
    , moveDown <| component.position.y
    ]


attrsMouseInteraction : Component -> List (Attribute Msg)
attrsMouseInteraction component =
    [ htmlAttribute <| Html.Events.onMouseEnter <| MouseEnterComponent component.id
    , htmlAttribute <| Html.Events.onMouseLeave <| MouseLeaveComponent component.id
    ]


viewComponentInMenu : Model -> ID -> Element Msg
viewComponentInMenu model id =
    let
        component =
            getComponent model.componentsInMenu id

        idDragging =
            componentDragging model id
    in
    el
        [ width <| px <| component.size.x * sizeDefault
        , height <| px <| component.size.y * sizeDefault
        , Border.width 0
        , htmlAttribute <| Html.Attributes.style "cursor" "grab"
        , htmlAttribute <| Html.Attributes.style "transition" "transform 0.05s, opacity 0.3s ease"
        , Background.color <| rgb 0 0.8 0.5

        -- To make it draggable
        , htmlAttribute <| Draggable.mouseTrigger id DragMsg
        , htmlAttribute <| Html.Events.onMouseUp <| OnMouseDown id
        , htmlAttribute <|
            Html.Attributes.style "transform" <|
                "translate("
                    ++ String.fromFloat component.position.x
                    ++ "px, "
                    ++ String.fromFloat component.position.y
                    ++ "px)"

        -- Conditional attributes
        , htmlAttribute <|
            Html.Attributes.style "pointer-events"
                -- This is to detect mouseOver event also when
                -- the mouse is dragging stuff
                (if idDragging then
                    "none"

                 else
                    "auto"
                )
        , htmlAttribute <|
            Html.Attributes.style "z-index" <|
                if idDragging then
                    "1"

                else
                    "auto"
        , alpha <|
            if idDragging then
                0.8

            else
                1
        ]
    <|
        Maybe.withDefault emptyContent component.content


viewComponentInDashboard : Model -> Component -> Attribute Msg
viewComponentInDashboard model component =
    let
        isMouseOver =
            model.idComponentWithMouseOver == Just component.id
    in
    inFront <|
        el
            (attrsSize component
                ++ attrsPosition component
                ++ attrsMouseInteraction component
                ++ [ Border.color <| rgb 0.9 0.9 0.9
                   , clip

                   -- To make it draggable
                   , htmlAttribute <| Draggable.mouseTrigger component.id DragMsg
                   , htmlAttribute <| Html.Events.onMouseUp <| OnMouseDown component.id
                   , htmlAttribute <|
                        Html.Attributes.style "transform" <|
                            "translate("
                                ++ String.fromFloat component.position.x
                                ++ "px, "
                                ++ String.fromFloat component.position.y
                                ++ "px)"

                   -- Conditional attributes
                   , Background.color <|
                        if isMouseOver then
                            rgb 0.8 0.6 0.6

                        else
                            rgb 0.8 0.8 0.8
                   , inFront <|
                        if isMouseOver then
                            Input.button
                                [ alignRight
                                , alignTop
                                , Font.size 14
                                , moveDown 8
                                ]
                                { label = text "🗑️"
                                , onPress = Just <| RemoveComponent component.id
                                }

                        else
                            none
                   , htmlAttribute <|
                        Html.Attributes.style "cursor" <|
                            if isDragging model then
                                "no-drop"

                            else
                                "grab"
                   ]
            )
        <|
            Maybe.withDefault emptyContent component.content


viewComponentAsEmptyArea : Model -> Component -> Attribute Msg
viewComponentAsEmptyArea model component =
    {- Empty component are available to get dropped component.
       They are always of 1x1 size.
    -}
    inFront <|
        el
            (attrsSize component
                ++ attrsPosition component
                ++ attrsMouseInteraction component
                ++ [ Border.dashed
                   , Border.color <|
                        if model.idComponentWithMouseOver == Just component.id then
                            rgba 0.8 0.8 0.8 1

                        else
                            rgba 0 0 0 0
                   ]
            )
        <|
            none


viewComponentAsOverlay : Model -> Component -> Attribute Msg
viewComponentAsOverlay model component =
    {- Overlay is the dotted border component that give hints about were to drop
       a component
    -}
    inFront <|
        el
            (attrsSize component
                ++ attrsPosition component
                ++ [ Border.color <| rgb 0.4 0.4 0.4
                   , Border.dashed
                   , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                   ]
            )
        <|
            none


isDragging : { b | maybeDraggedComponent : Maybe a } -> Bool
isDragging model =
    case model.maybeDraggedComponent of
        Just _ ->
            True

        Nothing ->
            False



{-
   ██    ██ ██ ███████ ██     ██
   ██    ██ ██ ██      ██     ██
   ██    ██ ██ █████   ██  █  ██
    ██  ██  ██ ██      ██ ███ ██
     ████   ██ ███████  ███ ███
-}


view : Model -> Html Msg
view model =
    layout
        [ padding 10 ]
    <|
        column []
            [ if True then
                column
                    [ spacing 5
                    , paddingXY 0 10
                    ]
                    [ el [] <| text <| "maybeDraggedComponent: " ++ Debug.toString model.maybeDraggedComponent
                    , el [] <| text <| "isMouseUp: " ++ Debug.toString model.isMouseUp
                    , el [] <| text <| "dragStartPosition: " ++ Debug.toString model.dragStartPosition
                    , el [] <| text <| "drag: " ++ Debug.toString model.drag
                    , el [] <| text <| "componentsInMenu: " ++ Debug.toString model.componentsInMenu
                    , el [] <| text <| "componentsInDashboard: " ++ Debug.toString model.componentsInDashboard
                    , el [] <| text <| "componentsForEmpyAreas: " ++ (Debug.toString <| componentsForEmpyAreas model)
                    , el [] <| text <| "idComponentWithMouseOver: " ++ Debug.toString model.idComponentWithMouseOver
                    , link [] { label = text "Packery", url = "https://packery.metafizzy.co/" }
                    , link [] { label = text "zaboco/elm-draggable", url = "https://package.elm-lang.org/packages/zaboco/elm-draggable/latest" }
                    ]

              else
                none
            , row
                [ width fill
                ]
                [ {-
                     ███    ███ ███████ ███    ██ ██    ██
                     ████  ████ ██      ████   ██ ██    ██
                     ██ ████ ██ █████   ██ ██  ██ ██    ██
                     ██  ██  ██ ██      ██  ██ ██ ██    ██
                     ██      ██ ███████ ██   ████  ██████
                  -}
                  wrappedRow
                    [ padding <| borderWidthDefault * 2
                    , alignTop
                    , spacing <| borderWidthDefault * 2
                    , htmlAttribute <| Html.Attributes.style "z-index" "1"
                    , Background.color <| rgb 0.8 1 0.8
                    , inFront <|
                        if isDragging model then
                            el
                                [ width fill
                                , height fill
                                , Background.color <| rgba 1 1 1 0.5
                                , htmlAttribute <| Html.Attributes.style "cursor" "no-drop"
                                ]
                            <|
                                none

                        else
                            none
                    ]
                  <|
                    List.map (\component -> viewComponentInMenu model component.id) model.componentsInMenu

                {-
                   ██████   █████  ███████ ██   ██ ██████   ██████   █████  ██████  ██████
                   ██   ██ ██   ██ ██      ██   ██ ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
                   ██   ██ ███████ ███████ ███████ ██████  ██    ██ ███████ ██████  ██   ██
                   ██   ██ ██   ██      ██ ██   ██ ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
                   ██████  ██   ██ ███████ ██   ██ ██████   ██████  ██   ██ ██   ██ ██████
                -}
                , el
                    [ padding borderWidthDefault
                    , alignTop
                    , Background.color <| rgb 0.9 0.9 0.9
                    ]
                  <|
                    column
                        ([ width <| px <| dimensionDefault.x * sizeDefault
                         , height <| px <| dimensionDefault.y * sizeDefault
                         , alignTop
                         ]
                            ++ List.map (\component -> viewComponentInDashboard model component) model.componentsInDashboard
                            ++ List.map (\component -> viewComponentAsEmptyArea model component) (componentsForEmpyAreas model)
                            ++ (case getOverlay model of
                                    Just overlay ->
                                        [ viewComponentAsOverlay model overlay ]

                                    Nothing ->
                                        []
                               )
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

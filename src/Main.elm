module Main exposing (main)

import Array
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
import Time


debug : Bool
debug =
    True



{-
   ███    ███  █████  ████████ ██████  ██ ██   ██
   ████  ████ ██   ██    ██    ██   ██ ██  ██ ██
   ██ ████ ██ ███████    ██    ██████  ██   ███
   ██  ██  ██ ██   ██    ██    ██   ██ ██  ██ ██
   ██      ██ ██   ██    ██    ██   ██ ██ ██   ██
-}


type alias Matrix =
    Array.Array (Array.Array String)


matrixEmpty : Settings -> Matrix
matrixEmpty settings =
    Array.repeat settings.sizeDashboard.x (Array.repeat settings.sizeDashboard.y "")


type alias Point =
    ( Int, Int, String )


normalizePosition : Settings -> { a | position : Position } -> { a | position : Position }
normalizePosition settings component =
    let
        position =
            { x = component.position.x // settings.sizeCell
            , y = component.position.y // settings.sizeCell
            }
    in
    { component | position = position }


componentToPoints : { a | id : ID, size : Size } -> List Point
componentToPoints { id, size } =
    List.concat <|
        List.indexedMap
            (\indexX _ ->
                List.indexedMap (\indexY _ -> ( indexX, indexY, id )) (List.repeat size.y ())
            )
            (List.repeat size.x ())


movePoints : { a | position : Position } -> List Point -> List Point
movePoints { position } deco =
    List.map (\( x_, y_, id ) -> ( x_ + position.x, y_ + position.y, id )) deco


addPointToMatrix : Point -> Matrix -> Matrix
addPointToMatrix ( x, y, id ) matrix =
    let
        maybeRow =
            Array.get y matrix
    in
    case maybeRow of
        Just row ->
            let
                newRow =
                    Array.set x id row
            in
            Array.set y newRow matrix

        Nothing ->
            matrix


addPointsToMatrix : Matrix -> List Point -> Matrix
addPointsToMatrix matrix points =
    List.foldl addPointToMatrix matrix points


addComponentToMatrix : Settings -> { a | id : ID, position : Position, size : Size } -> Matrix -> Matrix
addComponentToMatrix settings { id, position, size } matrix =
    { id = id, size = size }
        |> componentToPoints
        |> movePoints (normalizePosition settings { position = position })
        |> addPointsToMatrix matrix


addComponentsToMatrix : Settings -> Matrix -> List { a | id : ID, position : Position, size : Size } -> Matrix
addComponentsToMatrix settings matrix components =
    List.foldl (addComponentToMatrix settings) matrix components


viewTest : Model -> Element Msg
viewTest model =
    let
        matrix =
            addComponentsToMatrix model.settings (matrixEmpty model.settings) model.components
    in
    column [ padding 30 ]
        [ column [ spacing 5 ] <|
            List.map
                (\row_ ->
                    row [ spacing 5 ] <|
                        List.map
                            (\cell ->
                                el
                                    [ width <| px 100
                                    , height <| px 35
                                    , Background.color <| rgb 0.8 0.8 0.8
                                    ]
                                <|
                                    el [ centerX, centerY ] <|
                                        text cell
                            )
                            (Array.toList row_)
                )
                (Array.toList matrix)
        ]


norPos : Settings -> Int -> Int
norPos settings pos =
    pos * settings.sizeCell


labelEmpty : String
labelEmpty =
    "empty"


type alias Position =
    { x : Int
    , y : Int
    }


type alias Size =
    { x : Int
    , y : Int
    }


type alias ID =
    String


type Location
    = Menu
    | Dashboard
    | None


type alias Component =
    { id : ID
    , position : Position
    , size : Size
    , content : Content
    , location : Location
    }


type Content
    = Empty
    | Star
    | Clock
    | ClockJapan
    | DragState


pad : String -> String
pad text =
    String.padLeft 2 '0' text


japanTimeZone : Time.Zone
japanTimeZone =
    Time.customZone (9 * 60) []


time : Time.Posix -> Time.Zone -> String
time posix timeZone =
    (String.fromInt <| Time.toHour timeZone posix)
        ++ ":"
        ++ pad (String.fromInt <| Time.toMinute timeZone posix)
        ++ ":"
        ++ pad (String.fromInt <| Time.toSecond timeZone posix)


viewContent : Model -> Content -> Element Msg
viewContent model content =
    case content of
        Empty ->
            none

        Star ->
            el [ centerX, centerY ] <| text "☆"

        ClockJapan ->
            column [ centerX, centerY, spacing 10 ] <|
                [ el [ centerX, Font.size 14 ] <| text "Japan Time"
                , el [ centerX ] <| text <| time model.posix japanTimeZone
                ]

        Clock ->
            column [ centerX, centerY, spacing 10 ] <|
                [ el [ centerX, Font.size 14 ] <| text "UTC Time"
                , el [ centerX ] <| text <| time model.posix Time.utc
                ]

        DragState ->
            el [ centerX, centerY, padding 10 ] <|
                paragraph
                    [ Font.size 13 ]
                    [ text <| Debug.toString model.drag ]


emptyComponent : Settings -> Component
emptyComponent settings =
    { id = ""
    , position = Position (norPos settings 0) (norPos settings 0)
    , size = Size 0 0
    , content = Empty
    , location = None
    }


type alias Settings =
    { sizeCell : Int
    , sizeDashboard : Size
    , sizeBorder : Int
    }


type alias Model =
    { components : List Component
    , draggedComponent : Maybe Component
    , isMouseUp : ID
    , idComponentWithMouseOver : Maybe ID
    , drag : Draggable.State String
    , dragStartPosition : Maybe Position
    , posix : Time.Posix

    -- SETTINGS
    , settings : Settings
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        settings =
            { sizeCell = 100
            , sizeDashboard = Size 5 5
            , sizeBorder = 5
            }
    in
    ( { components =
            [ { id = "Comp. 4", position = Position (norPos settings 0) (norPos settings 0), size = Size 2 1, content = ClockJapan, location = Menu }
            , { id = "Comp. 3", position = Position (norPos settings 0) (norPos settings 0), size = Size 2 2, content = Clock, location = Menu }
            , { id = "Comp. 1", position = Position (norPos settings 0) (norPos settings 0), size = Size 2 1, content = Star, location = Menu }
            , { id = "Comp. 2", position = Position (norPos settings 0) (norPos settings 0), size = Size 2 1, content = DragState, location = Menu }

            --
            -- DAHSBOARD
            --
            , { id = "Component 1", position = Position (norPos settings 0) (norPos settings 0), size = Size 1 2, content = Clock, location = Dashboard }
            , { id = "Component 2", position = Position (norPos settings 1) (norPos settings 0), size = Size 1 1, content = Star, location = Dashboard }
            , { id = "Component 3", position = Position (norPos settings 2) (norPos settings 0), size = Size 3 1, content = DragState, location = Dashboard }

            --, { id = "Component 4", position = Position (norPos settings 1) (norPos settings 1), size = Size 2 2, content = Empty, location = Dashboard }
            , { id = "Component 5", position = Position (norPos settings 0) (norPos settings 2), size = Size 1 1, content = ClockJapan, location = Dashboard }

            --, { id = "Component 6", position = Position (norPos settings 3) (norPos settings 1), size = Size 1 2, content = Empty, location = Dashboard }
            , { id = "Component 7", position = Position (norPos settings 4) (norPos settings 1), size = Size 1 1, content = Empty, location = Dashboard }
            , { id = "Component 8", position = Position (norPos settings 4) (norPos settings 2), size = Size 1 1, content = Empty, location = Dashboard }

            --, { id = "Component 9", position = Position (norPos settings 0) (norPos settings 3), size = Size 5 1, content = Empty, location = Dashboard }
            , { id = "Component 10", position = Position (norPos settings 0) (norPos settings 4), size = Size 4 1, content = Empty, location = Dashboard }
            , { id = "Component 11", position = Position (norPos settings 4) (norPos settings 4), size = Size 1 1, content = Empty, location = Dashboard }
            ]
      , drag = Draggable.init
      , draggedComponent = Nothing
      , isMouseUp = ""
      , idComponentWithMouseOver = Nothing
      , dragStartPosition = Nothing
      , posix = Time.millisToPosix 0

      -- SETTINGS
      , settings = settings
      }
    , Cmd.none
    )


positionToId : String -> Position -> ID
positionToId label { x, y } =
    label ++ "_" ++ String.fromInt x ++ "_" ++ String.fromInt y


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
                                Just <| Position (round x_) (round y_)

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            else
                Nothing


componentsForEmpyAreas : Model -> List Component
componentsForEmpyAreas model =
    let
        settings =
            model.settings
    in
    {- TODO
       This function should cover all empty areas automatically (see Matrix)
    -}
    [ { id = positionToId labelEmpty <| Position (norPos settings 1) (norPos settings 1), position = Position (norPos settings 1) (norPos settings 1), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 2) (norPos settings 1), position = Position (norPos settings 2) (norPos settings 1), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 3) (norPos settings 1), position = Position (norPos settings 3) (norPos settings 1), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 1) (norPos settings 2), position = Position (norPos settings 1) (norPos settings 2), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 2) (norPos settings 2), position = Position (norPos settings 2) (norPos settings 2), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 3) (norPos settings 2), position = Position (norPos settings 3) (norPos settings 2), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 0) (norPos settings 3), position = Position (norPos settings 0) (norPos settings 3), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 1) (norPos settings 3), position = Position (norPos settings 1) (norPos settings 3), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 2) (norPos settings 3), position = Position (norPos settings 2) (norPos settings 3), size = Size 1 1, content = Empty, location = None }
    , { id = positionToId labelEmpty <| Position (norPos settings 3) (norPos settings 3), position = Position (norPos settings 3) (norPos settings 3), size = Size 1 1, content = Empty, location = None }

    --, { id = positionToId labelEmpty <| Position (norPos settings 4) (norPos settings 3), position = Position (norPos settings 4) (norPos settings 3), size = Size 1 1, content = Nothing, location = None }
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
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model | posix = posix }, Cmd.none )

        RemoveComponent id ->
            let
                components =
                    List.filter (\component -> component.id /= id) model.components
            in
            ( { model | components = components }, Cmd.none )

        MouseEnterComponent id ->
            ( { model | idComponentWithMouseOver = Just id }, Cmd.none )

        MouseLeaveComponent id ->
            ( { model | idComponentWithMouseOver = Nothing }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                components =
                    case model.draggedComponent of
                        Just draggingComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggingComponent.id then
                                        { component
                                            | position =
                                                { x = component.position.x + round dx
                                                , y = component.position.y + round dy
                                                }
                                        }

                                    else
                                        component
                                )
                                model.components

                        Nothing ->
                            model.components
            in
            ( { model | components = components }, Cmd.none )

        OnDragStart id ->
            let
                component =
                    getComponent model.settings model.components id
            in
            ( { model
                | draggedComponent = Just component
              }
            , Cmd.none
            )

        OnDragEnd ->
            let
                idDraggedComponent =
                    case model.draggedComponent of
                        Just draggedComponent ->
                            draggedComponent.id

                        Nothing ->
                            ""

                xxx =
                    getComponent model.settings model.components idDraggedComponent

                components1 =
                    model.components

                components2 =
                    case getOverlay model of
                        Just overlay ->
                            { id = "new"
                            , position = overlay.position
                            , size = overlay.size
                            , content = xxx.content
                            , location = Dashboard
                            }
                                :: components1

                        Nothing ->
                            components1

                -- Put the component in the initial position, for the moment.
                components3 =
                    case model.draggedComponent of
                        Just draggedComponent ->
                            List.map
                                (\component ->
                                    if component.id == draggedComponent.id then
                                        { component
                                            | position =
                                                Position draggedComponent.position.x
                                                    draggedComponent.position.y
                                        }

                                    else
                                        component
                                )
                                components2

                        Nothing ->
                            components2
            in
            ( { model
                | draggedComponent = Nothing
                , components = components3
              }
            , Cmd.none
            )

        OnClick id ->
            ( { model | draggedComponent = Nothing }, Cmd.none )

        OnMouseDown id ->
            ( model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ Draggable.subscriptions DragMsg drag
        , Time.every 1000 Tick
        ]


isComponentDragging : Model -> ID -> Bool
isComponentDragging model id =
    case model.draggedComponent of
        Just draggingComponent ->
            draggingComponent.id == id

        Nothing ->
            False


getComponent : Settings -> List Component -> ID -> Component
getComponent settings components id =
    let
        temp =
            List.head (List.filter (\component_ -> component_.id == id) components)
    in
    case temp of
        Just component_ ->
            component_

        Nothing ->
            emptyComponent settings


getOverlay : Model -> Maybe Component
getOverlay model =
    let
        settings =
            model.settings
    in
    {- TODO
       This function should detect all available positions based on dragged item.
    -}
    if isDragging model then
        let
            component =
                getComponent model.settings model.components <| .id <| Maybe.withDefault (emptyComponent model.settings) model.draggedComponent

            id x y =
                Just (positionToId labelEmpty (Position (norPos settings x) (norPos settings y)))

            empty x y dx dy =
                Just { id = "Overlay", position = Position (norPos settings x) (norPos settings y), size = Size dx dy, content = Empty, location = None }
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


attrsSize : Settings -> Component -> List (Attribute Msg)
attrsSize settings component =
    [ width <| px <| component.size.x * settings.sizeCell
    , height <| px <| component.size.y * settings.sizeCell
    , Border.width settings.sizeBorder
    ]


attrsPosition : Component -> List (Attribute Msg)
attrsPosition component =
    [ moveRight <| toFloat component.position.x
    , moveDown <| toFloat component.position.y
    ]


attrsMouseInteraction : Component -> List (Attribute Msg)
attrsMouseInteraction component =
    [ htmlAttribute <| Html.Events.onMouseEnter <| MouseEnterComponent component.id
    , htmlAttribute <| Html.Events.onMouseLeave <| MouseLeaveComponent component.id
    ]


viewComponentInMenu : Model -> ID -> Element Msg
viewComponentInMenu model id =
    let
        settings =
            model.settings

        component =
            getComponent model.settings model.components id

        isThisDragging_ =
            isComponentDragging model id
    in
    el
        [ width <| px <| component.size.x * settings.sizeCell
        , height <| px <| component.size.y * settings.sizeCell
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
                    ++ String.fromInt component.position.x
                    ++ "px, "
                    ++ String.fromInt component.position.y
                    ++ "px)"

        -- Conditional attributes
        , htmlAttribute <|
            Html.Attributes.style "pointer-events"
                -- This is to detect mouseOver event also when
                -- the mouse is dragging stuff
                (if isThisDragging_ then
                    "none"

                 else
                    "auto"
                )
        , htmlAttribute <|
            Html.Attributes.style "z-index" <|
                if isThisDragging_ then
                    "1"

                else
                    "auto"
        , alpha <|
            if isThisDragging_ then
                0.8

            else
                1
        ]
    <|
        viewInnerComponent model component


viewComponentInDashboard : Model -> Component -> Attribute Msg
viewComponentInDashboard model component =
    let
        isMouseOver =
            model.idComponentWithMouseOver == Just component.id

        isThisDragging_ =
            isComponentDragging model component.id
    in
    inFront <|
        el
            (attrsSize model.settings component
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
                                ++ String.fromInt component.position.x
                                ++ "px, "
                                ++ String.fromInt component.position.y
                                ++ "px)"

                   -- Conditional attributes
                   , htmlAttribute <|
                        Html.Attributes.style "z-index" <|
                            if isThisDragging_ then
                                "1"

                            else
                                "auto"
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
            viewInnerComponent model component


viewInnerComponent : Model -> Component -> Element Msg
viewInnerComponent model component =
    column [ height fill, width fill ]
        [ viewContent model component.content
        , el
            [ alignRight
            , alignBottom
            , Font.size 12
            , padding 2
            , Font.color <| rgb 0.4 0.4 0.4
            ]
          <|
            text component.id
        ]


viewComponentAsEmptyArea : Model -> Component -> Attribute Msg
viewComponentAsEmptyArea model component =
    {- Empty component are available to get dropped component.
       They are always of 1x1 size.
    -}
    inFront <|
        el
            (attrsSize model.settings component
                ++ attrsPosition component
                ++ attrsMouseInteraction component
                ++ [ Border.color <|
                        if model.idComponentWithMouseOver == Just component.id && debug then
                            rgba 0.6 0.8 0.8 1

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
            (attrsSize model.settings component
                ++ attrsPosition component
                ++ [ Border.color <| rgb 0.4 0.4 0.4
                   , Border.dashed
                   , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                   ]
            )
        <|
            none


isDragging : { b | draggedComponent : Maybe a } -> Bool
isDragging model =
    case model.draggedComponent of
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
    let
        settings =
            model.settings
    in
    layout
        []
    <|
        column
            [ centerX
            , centerY
            ]
            [ if debug then
                column
                    [ spacing 5
                    , paddingXY 0 10
                    , Font.size 13
                    , padding 10
                    , width <| px 740

                    --, height <| px 120
                    , Border.width 1
                    , scrollbarY
                    , scrollbarX
                    ]
                    [ viewTest model
                    , el [] <| text <| "· draggedComponent: " ++ Debug.toString model.draggedComponent
                    , el [] <| text <| "· isMouseUp: " ++ Debug.toString model.isMouseUp
                    , el [] <| text <| "· dragStartPosition: " ++ Debug.toString model.dragStartPosition
                    , el [] <| text <| "· drag: " ++ Debug.toString model.drag

                    --, el [] <| text <| "components: " ++ Debug.toString model.components
                    , el [] <| text <| "· components quantity: " ++ (Debug.toString <| List.length model.components)

                    --, el [] <| text <| "· componentsForEmpyAreas: " ++ (Debug.toString <| componentsForEmpyAreas model)
                    , el [] <| text <| "· idComponentWithMouseOver: " ++ Debug.toString model.idComponentWithMouseOver
                    , link [] { label = text "· Packery", url = "https://packery.metafizzy.co/" }
                    , link [] { label = text "· zaboco/elm-draggable", url = "https://package.elm-lang.org/packages/zaboco/elm-draggable/latest" }
                    ]

              else
                none
            , column []
                [ text "Settings"
                , text """            { sizeCell = 100
                        , sizeDashboard = Size 5 5
                        , sizeBorder = 5
                        }"""
                ]
            , row
                []
                [ {-
                     ███    ███ ███████ ███    ██ ██    ██
                     ████  ████ ██      ████   ██ ██    ██
                     ██ ████ ██ █████   ██ ██  ██ ██    ██
                     ██  ██  ██ ██      ██  ██ ██ ██    ██
                     ██      ██ ███████ ██   ████  ██████
                  -}
                  column
                    [ padding <| settings.sizeBorder * 2
                    , alignTop
                    , spacing <| settings.sizeBorder * 2
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
                    List.map (\component -> viewComponentInMenu model component.id) (List.filter (\c -> c.location == Menu) model.components)

                {-
                   ██████   █████  ███████ ██   ██ ██████   ██████   █████  ██████  ██████
                   ██   ██ ██   ██ ██      ██   ██ ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
                   ██   ██ ███████ ███████ ███████ ██████  ██    ██ ███████ ██████  ██   ██
                   ██   ██ ██   ██      ██ ██   ██ ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
                   ██████  ██   ██ ███████ ██   ██ ██████   ██████  ██   ██ ██   ██ ██████
                -}
                , el
                    [ padding settings.sizeBorder
                    , alignTop
                    , Background.color <| rgb 0.9 0.9 0.9
                    ]
                  <|
                    column
                        ([ width <| px <| settings.sizeDashboard.x * settings.sizeCell
                         , height <| px <| settings.sizeDashboard.y * settings.sizeCell
                         , alignTop
                         ]
                            ++ List.map (\component -> viewComponentInDashboard model component) (List.filter (\c -> c.location == Dashboard) model.components)
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

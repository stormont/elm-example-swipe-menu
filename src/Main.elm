module Main exposing (main)

{-|
  Swipeable Menu
  https://ellie-app.com/ddf74tk6Zhxa1
-}

import Animation exposing (percent, px, turn)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import List.Extra


darkBlue =
    Element.rgb 0 0 0.9


swipeSize =
    100


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type MenuItemState
    = Init
    | LeftOption
    | RightOption


initMenuItem index text =
    { state = Init
    , index = index
    , text = text
    , leftSwipeOffset = 0
    , rightSwipeOffset = 0
    , swipeOffset = 0
    }


init =
    { swipingIndex = Nothing
    , swipeStartPos = Nothing
    , swipeCurrentPos = Nothing
    , menuItems =
        [ initMenuItem 0 "Menu Item 1"
        , initMenuItem 1 "Menu Item 2"
        ]
    }



-- Detecting a pointer event that ends off-screen requires the use
-- of ports; see: https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/latest/


type Msg
    = NoOp
    | HorizontalSwipeStartAt Int ( Float, Float )
    | HorizontalSwipeMoveAt ( Float, Float )
    | HorizontalSwipeEndAt ( Float, Float )


initSwipe elem =
    case elem.state of
        Init ->
            { elem
                | leftSwipeOffset = 0
                , rightSwipeOffset = 0
                , swipeOffset = 0
            }

        LeftOption ->
            { elem
                | leftSwipeOffset = swipeSize
                , rightSwipeOffset = 0
                , swipeOffset = swipeSize
            }

        RightOption ->
            { elem
                | leftSwipeOffset = 0
                , rightSwipeOffset = swipeSize
                , swipeOffset = -swipeSize
            }


updateOffset elem startX endX =
    let
        offset =
            startX
                - endX
                |> abs
                |> min swipeSize
    in
    case elem.state of
        Init ->
            if endX < startX then
                ( offset, 0, offset )

            else
                ( 0, offset, -offset )

        LeftOption ->
            if endX < startX then
                ( swipeSize, 0, swipeSize )

            else
                ( swipeSize - offset, 0, swipeSize - offset )

        RightOption ->
            if endX < startX then
                ( 0, swipeSize - offset, offset - swipeSize )

            else
                ( 0, swipeSize, -swipeSize )


finalizeSwipe elem startX endX =
    let
        latchLeft =
            startX - endX >= swipeSize

        latchRight =
            endX - startX >= swipeSize
    in
    case elem.state of
        Init ->
            case ( latchLeft, latchRight ) of
                ( True, _ ) ->
                    { elem
                        | state = LeftOption
                        , leftSwipeOffset = swipeSize
                        , rightSwipeOffset = 0
                        , swipeOffset = swipeSize
                    }

                ( _, True ) ->
                    { elem
                        | state = RightOption
                        , leftSwipeOffset = 0
                        , rightSwipeOffset = swipeSize
                        , swipeOffset = -swipeSize
                    }

                _ ->
                    { elem
                        | state = Init
                        , leftSwipeOffset = 0
                        , rightSwipeOffset = 0
                        , swipeOffset = 0
                    }

        LeftOption ->
            case latchLeft of
                True ->
                    { elem
                        | state = LeftOption
                        , leftSwipeOffset = swipeSize
                        , rightSwipeOffset = 0
                        , swipeOffset = swipeSize
                    }

                _ ->
                    { elem
                        | state = Init
                        , leftSwipeOffset = 0
                        , rightSwipeOffset = 0
                        , swipeOffset = 0
                    }

        RightOption ->
            case latchRight of
                True ->
                    { elem
                        | state = RightOption
                        , leftSwipeOffset = 0
                        , rightSwipeOffset = swipeSize
                        , swipeOffset = -swipeSize
                    }

                _ ->
                    { elem
                        | state = Init
                        , leftSwipeOffset = 0
                        , rightSwipeOffset = 0
                        , swipeOffset = 0
                    }


update msg model =
    case msg of
        NoOp ->
            model

        HorizontalSwipeStartAt index coords ->
            case List.Extra.getAt index model.menuItems of
                Nothing ->
                    model

                Just elem ->
                    { model
                        | swipingIndex = Just index
                        , swipeStartPos = Just coords
                        , swipeCurrentPos = Just coords
                        , menuItems =
                            List.Extra.setAt
                                index
                                (initSwipe elem)
                                model.menuItems
                    }

        HorizontalSwipeMoveAt ( endX, endY ) ->
            case ( model.swipingIndex, model.swipeStartPos ) of
                ( Just index, Just ( startX, _ ) ) ->
                    case List.Extra.getAt index model.menuItems of
                        Nothing ->
                            { model
                                | swipeCurrentPos = Just ( endX, endY )
                            }

                        Just elem ->
                            let
                                ( leftSwipeOffset, rightSwipeOffset, swipeOffset ) =
                                    updateOffset elem startX endX
                            in
                            { model
                                | swipeCurrentPos = Just ( endX, endY )
                                , menuItems =
                                    List.Extra.setAt
                                        index
                                        { elem
                                            | leftSwipeOffset = leftSwipeOffset
                                            , rightSwipeOffset = rightSwipeOffset
                                            , swipeOffset = swipeOffset
                                        }
                                        model.menuItems
                            }

                _ ->
                    model

        HorizontalSwipeEndAt ( endX, endY ) ->
            case ( model.swipingIndex, model.swipeStartPos ) of
                ( Just index, Just ( startX, startY ) ) ->
                    let
                        updatedElems =
                            case List.Extra.getAt index model.menuItems of
                                Nothing ->
                                    model.menuItems

                                Just elem ->
                                    List.Extra.setAt
                                        index
                                        (finalizeSwipe elem startX endX)
                                        model.menuItems
                    in
                    { model
                        | swipingIndex = Nothing
                        , swipeStartPos = Nothing
                        , swipeCurrentPos = Nothing
                        , menuItems = updatedElems
                    }

                _ ->
                    { model
                        | swipingIndex = Nothing
                        , swipeStartPos = Nothing
                        , swipeCurrentPos = Nothing
                    }


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


horizontalPointerEvents =
    [ htmlAttribute <| Touch.onMove (HorizontalSwipeMoveAt << touchCoordinates)
    , htmlAttribute <| Touch.onEnd (HorizontalSwipeEndAt << touchCoordinates)
    , htmlAttribute <| Mouse.onMove (\event -> HorizontalSwipeMoveAt event.clientPos)
    , htmlAttribute <| Mouse.onUp (\event -> HorizontalSwipeEndAt event.clientPos)
    ]


renderMenuItem elem =
    row
        [ width fill
        , Border.color darkBlue
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }

        -- Track start swipes from the touched element
        , htmlAttribute <| Touch.onStart (HorizontalSwipeStartAt elem.index << touchCoordinates)
        , htmlAttribute <| Mouse.onDown (\event -> HorizontalSwipeStartAt elem.index event.clientPos)
        ]
        [ el
            [ height fill
            , width (Element.px swipeSize)
            , moveRight (elem.rightSwipeOffset - swipeSize)
            , Background.color (Element.rgba 1.0 0 0 <| elem.rightSwipeOffset / swipeSize)
            ]
            (el
                [ centerX
                , centerY
                , padding 0
                ]
                (text "x")
            )
        , el
            [ width fill
            , moveLeft elem.swipeOffset
            ]
            (el
                [ padding 10
                , centerX
                , centerY
                ]
                (text elem.text)
            )
        , el
            [ height fill
            , width (Element.px swipeSize)
            , moveLeft (elem.leftSwipeOffset - swipeSize)
            , Background.color (Element.rgba 0 1.0 0 <| elem.leftSwipeOffset / swipeSize)
            ]
            (el
                [ centerX
                , centerY
                , padding 0
                ]
                (text "o")
            )
        ]


view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column
            ([ width fill
             , height shrink

             -- Need to track swipe at a higher DOM to track when the movement goes off the start element
             ]
                ++ horizontalPointerEvents
            )
            ([ el
                [ width fill
                , Border.color darkBlue
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]
                (el
                    [ padding 10
                    , centerX
                    , centerY
                    , Font.size 36
                    ]
                    (text "Swipeable Menu")
                )
             ]
                ++ List.map renderMenuItem model.menuItems
            )

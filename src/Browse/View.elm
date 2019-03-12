module Browse.View exposing (root)

import Array exposing (Array)
import Browse.Types exposing (..)
import Contracts.Types
import Element
import Element.Border
import Element.Events
import Element.Font
import ElementHelpers as EH
import List
import Time
import TimeHelpers
import TokenValue


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    viewList time model.trades


viewList : Time.Posix -> Array TTListItem -> Element.Element Msg
viewList time trades =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        (trades
            |> Array.toList
            |> List.filter (itemFilter time)
            |> List.map (viewListItem time)
        )


itemFilter : Time.Posix -> TTListItem -> Bool
itemFilter time item =
    case ( item.parameters, item.state ) of
        ( Nothing, _ ) ->
            False

        ( _, Nothing ) ->
            False

        ( Just parameters, Just state ) ->
            state.phase
                == Contracts.Types.Open
                && (not <| isAutorecallLocked time parameters state)


isAutorecallLocked : Time.Posix -> Contracts.Types.CreateParameters -> Contracts.Types.State -> Bool
isAutorecallLocked currentTime parameters state =
    let
        lockTime =
            TimeHelpers.add
                state.phaseStartTime
                parameters.autorecallInterval

        timeLeft =
            TimeHelpers.sub
                lockTime
                currentTime
    in
    TimeHelpers.isNegative timeLeft


viewListItem : Time.Posix -> TTListItem -> Element.Element Msg
viewListItem time info =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 100)
        , Element.Border.width 1
        , Element.Border.rounded 10
        , Element.Events.onClick (ItemClicked info.id)
        , Element.pointer
        ]
        (case ( info.address, info.parameters, info.state ) of
            ( Just _, Just parameters, Just state ) ->
                Element.row
                    [ Element.padding 10
                    , Element.spacing 20
                    , Element.height Element.fill
                    , Element.width Element.fill
                    ]
                    [ Element.column
                        [ Element.width (Element.fillPortion 1)
                        , Element.centerY
                        , Element.Border.width 1
                        , Element.spacing 3
                        ]
                        [ Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text ((parameters.tradeAmount |> TokenValue.renderToString (Just 2)) ++ " Dai"))
                        , Element.el [ Element.centerX, Element.Font.size 16, Element.Font.italic ] (Element.text "for")
                        , Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text parameters.totalPriceString)
                        ]
                    , Element.column
                        [ Element.width (Element.fillPortion 1)
                        , Element.centerY
                        , Element.Border.width 1
                        , Element.spacing 3
                        ]
                        [ Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text "Expires in")
                        , Element.el [ Element.centerX, Element.Font.size 20 ]
                            (Element.text
                                (EH.secondsRemainingString
                                    (TimeHelpers.add state.phaseStartTime parameters.autorecallInterval)
                                    time
                                )
                            )
                        ]
                    , Element.el
                        [ Element.width (Element.fillPortion 6)
                        , Element.height Element.fill
                        , Element.Border.width 1
                        ]
                        (Element.paragraph [] [ Element.text "displaying transerMethods currently broken" ])
                    , Element.column
                        [ Element.width (Element.fillPortion 2)
                        , Element.centerY
                        , Element.spacing 10
                        ]
                        [ Element.el [ Element.centerX, Element.Font.size 18 ]
                            (Element.text
                                ("Auto-abort time: "
                                    ++ TimeHelpers.toString parameters.autoabortInterval
                                )
                            )
                        , Element.el [ Element.centerX, Element.Font.size 18 ]
                            (Element.text
                                ("Auto-release time: "
                                    ++ TimeHelpers.toString parameters.autoreleaseInterval
                                )
                            )
                        ]
                    ]

            ( Nothing, _, _ ) ->
                loadingMessage "Loading address..."

            ( _, Nothing, _ ) ->
                loadingMessage "Loading parameters..."

            ( _, _, Nothing ) ->
                loadingMessage "Loading state..."
        )


loadingMessage : String -> Element.Element Msg
loadingMessage s =
    Element.el
        [ Element.centerX, Element.centerY, Element.Font.size 24 ]
        (Element.text s)

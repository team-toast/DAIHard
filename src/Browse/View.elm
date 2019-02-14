module Browse.View exposing (root)

import Array exposing (Array)
import Browse.Types exposing (..)
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
    viewList time model.ttArray


viewList : Time.Posix -> Array TTListItem -> Element.Element Msg
viewList time ttArray =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        (ttArray
            |> Array.toList
            |> List.map (viewListItem time)
        )


viewListItem : Time.Posix -> TTListItem -> Element.Element Msg
viewListItem time info =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 100)
        , Element.Border.width 1
        , Element.Border.rounded 10
        , Element.Events.onClick (ItemClicked info.id)
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
                        [ Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text ((parameters.uncoiningAmount |> TokenValue.renderToString (Just 2)) ++ " Dai"))
                        , Element.el [ Element.centerX, Element.Font.size 16, Element.Font.italic ] (Element.text "for")
                        , Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text ("$" ++ (parameters.price |> TokenValue.renderToString (Just 2))))
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
                        (Element.paragraph [] [ Element.text parameters.transferMethods ])
                    , Element.column
                        [ Element.width (Element.fillPortion 2)
                        , Element.centerY
                        , Element.spacing 10
                        ]
                        [ Element.el [ Element.centerX, Element.Font.size 18 ]
                            (Element.text
                                ("Auto-abort time: "
                                    ++ TimeHelpers.toString parameters.depositDeadlineInterval
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

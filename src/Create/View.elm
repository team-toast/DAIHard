module Create.View exposing (root)

import Contracts.Types
import Create.Types exposing (..)
import Element
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import RenderContract.Types
import RenderContract.View
import TransferMethods


root : Model -> Element.Element Msg
root model =
    let
        contractRendered =
            case model.contractParameters of
                Nothing ->
                    Element.text "no contract?????"

                Just parameters ->
                    RenderContract.View.render RenderContract.Types.Draft parameters
                        |> Element.map (\_ -> NoOp)

        createButton =
            Element.Input.button [ Element.centerX, Element.Font.size 24 ]
                { onPress = Just BeginCreateProcess
                , label = Element.text "Create!"
                }
    in
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ titleElement model
        , contractParametersFormElement model
        , createButton
        , Element.el [ Element.paddingXY 150 0 ] contractRendered
        , createButton
        ]


titleElement : Model -> Element.Element Msg
titleElement model =
    Element.el
        [ Element.centerX
        , Element.Font.size 36
        , Element.Events.onClick SwitchInitiatorRole
        , Element.pointer
        ]
        (Element.text
            (case model.parameterInputs.openMode of
                Contracts.Types.BuyerOpened ->
                    "Opening as Buyer"

                Contracts.Types.SellerOpened ->
                    "Opening as Seller"
            )
        )


contractParametersFormElement : Model -> Element.Element Msg
contractParametersFormElement model =
    EH.fillWidthBlock "Contract parameters" (contractParametersForm model)


contractParametersForm : Model -> Element.Element Msg
contractParametersForm model =
    let
        columnHeader title =
            Element.el [ Element.Font.size 24, Element.Font.bold, Element.centerX ] (Element.text title)

        daiAmountInputs =
            let
                nameAndElementToRow tuple =
                    Element.row [ Element.spacing 8 ]
                        [ Element.el [ Element.width (Element.px 200) ]
                            (Element.paragraph [ Element.width Element.shrink, Element.alignLeft ] [ Element.text (Tuple.first tuple) ])
                        , Tuple.second tuple
                        ]
            in
            Element.column [ Element.width (Element.fillPortion 1), Element.spacing 8, Element.alignTop ]
                (columnHeader "Dai Amounts"
                    :: ([ ( "Trade Amount", EH.smallInput "tradeAmount" model.parameterInputs.tradeAmount TradeAmountChanged )
                        , ( "Total Fiat Price", EH.smallInput "summonfee" model.parameterInputs.totalPrice PriceChanged )
                        ]
                            |> List.map nameAndElementToRow
                       )
                )

        transferMethodsInput =
            Element.column [ Element.width (Element.fillPortion 3), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Fiat Transfer Methods"
                , Element.column [ Element.width Element.fill, Element.spacing 8 ]
                    [ Element.Input.button [ Element.centerX ]
                        { onPress =
                            Just <|
                                AddTransferMethod <|
                                    TransferMethods.CashDrop
                                        { location = TransferMethods.Location 10 10
                                        , radius = 10
                                        , description = Nothing
                                        }
                        , label = Element.text "Cash Drop"
                        }
                    , Element.Input.button [ Element.centerX ]
                        { onPress =
                            Just <|
                                AddTransferMethod <|
                                    TransferMethods.CashHandoff
                                        { location = TransferMethods.Location 10 10
                                        , radius = 10
                                        , description = Just "I can meet you at Central Square."
                                        }
                        , label = Element.text "Cash handoff"
                        }
                    , Element.Input.button [ Element.centerX ]
                        { onPress =
                            Just <|
                                AddTransferMethod <|
                                    TransferMethods.BankTransfer
                                        { identifierType = TransferMethods.Name
                                        , info = "National Bank of America"
                                        }
                        , label = Element.text "Bank transfer"
                        }
                    , Element.Input.button [ Element.centerX ]
                        { onPress =
                            Just <|
                                AddTransferMethod <|
                                    TransferMethods.Custom "wire it to me"
                        , label = Element.text "Custom 'wire it to me'"
                        }
                    ]
                ]

        intervalInputs =
            let
                nameAndElementToReversedRow tuple =
                    Element.row [ Element.spacing 8 ]
                        [ Tuple.second tuple
                        , Element.el [ Element.width (Element.px 170) ]
                            (Element.paragraph [ Element.width Element.shrink, Element.alignLeft ] [ Element.text (Tuple.first tuple) ])
                        ]
            in
            Element.column [ Element.width (Element.fillPortion 1), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Phase Time Limits"
                , Element.column [ Element.spacing 8 ]
                    ([ ( "Autorecall", EH.timeInput "autorecall interval" model.parameterInputs.autorecallInterval AutorecallIntervalChanged )
                     , ( "Autoabort", EH.timeInput "autoabort interval" model.parameterInputs.autoabortInterval AutoabortIntervalChanged )
                     , ( "Autorelease", EH.timeInput "autorelease interval" model.parameterInputs.autoreleaseInterval AutoreleaseIntervalChanged )
                     ]
                        |> List.map nameAndElementToReversedRow
                    )
                ]
    in
    Element.row [ Element.width Element.fill, Element.spacing 20 ]
        [ daiAmountInputs
        , transferMethodsInput
        , intervalInputs
        ]

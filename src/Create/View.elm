module Create.View exposing (root)

import Create.Types exposing (..)
import Element
import Element.Font
import Element.Input
import ElementHelpers as EH
import RenderContract.Types
import RenderContract.View


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
        [ contractParametersFormElement model
        , createButton
        , contractRendered
        , createButton
        ]


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
                    :: ([ ( "Uncoining Amount", EH.smallInput "uncoiningAmount" model.parameterInputs.uncoiningAmount UncoiningAmountChanged )
                        , ( "Total Fiat Price", EH.smallInput "summonfee" model.parameterInputs.price PriceChanged )
                        ]
                            |> List.map nameAndElementToRow
                       )
                )

        transferMethodsInput =
            Element.column [ Element.width (Element.fillPortion 3), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Fiat Transfer Methods"
                , Element.Input.multiline [ Element.width Element.fill, Element.height (Element.px 150) ]
                    { onChange = TransferMethodsChanged
                    , text = model.parameterInputs.transferMethods
                    , placeholder = Just (Element.Input.placeholder [] (Element.paragraph [] [ Element.text "Be specific, and consider listing multiple options. Keep in mind that many Responders find offers via keyword searches." ]))
                    , label = Element.Input.labelHidden "transferMethods"
                    , spellcheck = False
                    }
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
                     , ( "Deposit Deadline", EH.timeInput "deposit deadline interval" model.parameterInputs.depositDeadlineInterval DepositDeadlineIntervalChanged )
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

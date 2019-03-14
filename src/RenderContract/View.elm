module RenderContract.View exposing (render)

import Contracts.Types exposing (OpenMode(..))
import Element
import Element.Background
import Element.Border
import Element.Font
import ElementHelpers as EH
import Eth.Utils
import Flip exposing (flip)
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import RenderContract.Types exposing (..)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


render : ViewMode -> Contracts.Types.CreateParameters -> Element.Element Msg
render viewMode parameters =
    let
        postCommitBalance =
            TokenValue.add parameters.tradeAmount parameters.buyerDeposit

        abortBurnAmount =
            TokenValue.divByInt parameters.buyerDeposit 2

        totalTime =
            TimeHelpers.add parameters.autorecallInterval parameters.autoabortInterval
                |> TimeHelpers.add parameters.autoreleaseInterval

        titleElement =
            Element.column [ Element.centerX ]
                [ Element.el
                    [ Element.Font.size 42
                    , Element.centerX
                    ]
                    (Element.text "Trade Contract")
                , case viewMode of
                    Draft ->
                        Element.el [ Element.Font.size 36, Element.Font.light, Element.Font.italic, Element.centerX ]
                            (Element.text "Draft")

                    Active context ->
                        Element.el [ Element.Font.size 36, Element.Font.italic, Element.centerX ]
                            (Element.text (Contracts.Types.phaseToString context.state.phase))
                ]

        mainParametersElement =
            Element.column
                [ Element.Font.size 24
                , Element.Font.italic
                ]
                [ Element.paragraph []
                    [ case parameters.openMode of
                        BuyerOpened ->
                            Element.text "Buyer"

                        SellerOpened ->
                            Element.text "Seller"
                    , Element.text (": " ++ Eth.Utils.addressToString parameters.initiatorAddress)
                    ]
                , Element.paragraph []
                    [ Element.text
                        (case parameters.openMode of
                            BuyerOpened ->
                                "Buying "

                            SellerOpened ->
                                "Selling "
                        )
                    , EH.tokenValue parameters.tradeAmount
                    , Element.text " for "
                    , EH.fiatValue parameters.fiatPrice
                    ]
                , Element.paragraph []
                    [ Element.text "to be finalized within "
                    , EH.timeValue totalTime
                    , Element.text " of contract creation."
                    ]
                ]
    in
    Element.el
        [ Element.paddingXY 10 0
        , Element.width Element.fill
        ]
        (Element.column
            [ Element.padding 60
            , Element.spacing 40
            , Element.Border.rounded 3
            , Element.Border.color EH.contractBorderColor
            , Element.Border.width 1
            , Element.width Element.fill
            , Element.Background.color EH.contractBackgroundColor
            , EH.contractShadowAttribute
            ]
            [ titleElement
            , mainParametersElement
            , openPhaseElement viewMode parameters
            , committedPhaseElement viewMode parameters postCommitBalance abortBurnAmount
            , claimedPhaseElement viewMode parameters postCommitBalance
            ]
        )


openPhaseElement : ViewMode -> Contracts.Types.CreateParameters -> Element.Element Msg
openPhaseElement viewMode parameters =
    let
        ( initiator, responder ) =
            case parameters.openMode of
                BuyerOpened ->
                    ( EH.buyer, EH.seller )

                SellerOpened ->
                    ( EH.seller, EH.buyer )

        responderDeposit =
            case parameters.openMode of
                BuyerOpened ->
                    parameters.tradeAmount

                SellerOpened ->
                    parameters.buyerDeposit
    in
    Element.column (phaseStyleWithViewMode Contracts.Types.Open viewMode)
        [ phaseHeading Contracts.Types.Open parameters viewMode
        , EH.clauseList
            [ Element.paragraph []
                [ Element.text "This contract is listed on "
                , EH.fakeLink "CoinerTool browse"
                , Element.text " (and possibly other third-party tools)."
                ]
            , Element.paragraph []
                [ Element.text "Any other Ethereum user may become the "
                , responder []
                , Element.text " by depositing "
                , EH.tokenValue responderDeposit
                , Element.text " into this contract. This immediately moves the contract to the "
                , EH.sectionReference "Committed Phase"
                , Element.text ". Thenceforth, the identity of the "
                , responder []
                , Element.text " will never change, and the "
                , initiator []
                , Element.text " will be unable to "
                , EH.methodName "recall"
                , Element.text " the trade."
                ]
            , Element.paragraph []
                [ Element.text "Until and unless an Ethereum user "
                , EH.methodName "commit"
                , Element.text "s, the "
                , initiator []
                , Element.text " may execute the "
                , EH.methodName "recall"
                , Element.text " method. This "
                , EH.sectionReference "cancels the contract"
                , Element.text " and refunds the "
                , initiator []
                , Element.text "'s "
                , EH.tokenValue parameters.tradeAmount
                , Element.text ". "
                ]
            , Element.paragraph []
                [ Element.text "If no Ethereum user "
                , EH.methodName "commit"
                , Element.text "s within "
                , EH.timeValue parameters.autorecallInterval
                , Element.text ", "
                , EH.methodName "recall"
                , Element.text " will be automatically triggered."
                ]
            ]
        , case viewMode of
            Draft ->
                Element.none

            Active context ->
                if context.state.phase == Contracts.Types.Open then
                    Element.row [ Element.spacing 50, Element.padding 20, Element.centerX ]
                        (if context.userIsInitiator then
                            [ EH.contractActionButton "Recall" EH.buttonBlue Recall ]

                         else
                            [ EH.contractActionButton "Commit" EH.buttonBlue Commit ]
                        )

                else
                    Element.none
        ]


committedPhaseElement : ViewMode -> Contracts.Types.CreateParameters -> TokenValue -> TokenValue -> Element.Element Msg
committedPhaseElement viewMode parameters postCommitBalance claimFailBurnAmount =
    Element.column (phaseStyleWithViewMode Contracts.Types.Committed viewMode)
        [ phaseHeading Contracts.Types.Committed parameters viewMode
        , EH.clauseList
            [ Element.paragraph []
                [ Element.text "The contract has two parties ("
                , EH.seller []
                , Element.text " and "
                , EH.buyer []
                , Element.text "), and a total balance of "
                , EH.tokenValue postCommitBalance
                , Element.text "."
                ]
            , Element.paragraph []
                [ Element.text "The "
                , EH.buyer []
                , Element.text " is expected to transfer "
                , EH.fiatValue parameters.fiatPrice
                , Element.text " to the "
                , EH.seller []
                , Element.text " according to the following "
                , EH.sectionReference "Fiat Transfer Methods"
                , Element.text ", then mark the deposit as complete by calling "
                , EH.methodName "claim"
                , Element.text ". This immediately moves the contract to the "
                , EH.sectionReference "Claimed Phase"
                , Element.text "."
                ]
            , fiatPaymentMethodsElement parameters.paymentMethods
            , Element.paragraph []
                [ Element.text "The "
                , EH.seller []
                , Element.text " and "
                , EH.buyer []
                , Element.text " are expected to work together to resolve any ambiguity or difficulty regarding the fiat transfer "
                , Element.el [ Element.Font.bold ] (Element.text "before")
                , Element.text " the "
                , EH.buyer []
                , Element.text " calls "
                , EH.methodName "claim"
                , Element.text "."
                ]
            , Element.paragraph []
                [ Element.text "If the "
                , EH.buyer []
                , Element.text " does not "
                , EH.methodName "claim"
                , Element.text " within "
                , EH.timeValue parameters.autoabortInterval
                , Element.text ", "
                , EH.sectionReference "the contract is closed"
                , Element.text ", and the balance of "
                , EH.tokenValue postCommitBalance
                , Element.text " is handled as follows:"
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "Half of the "
                        , EH.buyer []
                        , Element.text "’s deposit ("
                        , EH.tokenValue claimFailBurnAmount
                        , Element.text ") is burned and half ("
                        , EH.tokenValue claimFailBurnAmount
                        , Element.text ") is returned to the "
                        , EH.buyer []
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.seller []
                        , Element.text " faces the same punishment ("
                        , EH.tokenValue claimFailBurnAmount
                        , Element.text " burned), and the rest of his deposit ("
                        , EH.tokenValue (TokenValue.sub parameters.tradeAmount claimFailBurnAmount)
                        , Element.text ") is refunded."
                        ]
                    ]
                ]
            ]
        , case viewMode of
            Draft ->
                Element.none

            Active context ->
                if context.state.phase == Contracts.Types.Committed then
                    Element.row [ Element.spacing 50, Element.padding 20, Element.centerX ]
                        (if context.userIsBuyer then
                            [ EH.contractActionButton "Claim" EH.buttonGreen Claim
                            , EH.contractActionButton "Abort" EH.buttonRed Abort
                            ]

                         else
                            []
                        )

                else
                    Element.none
        ]


fiatPaymentMethodsElement : List PaymentMethod -> Element.Element Msg
fiatPaymentMethodsElement paymentMethods =
    Element.column [ Element.width Element.fill, Element.spacing 5 ]
        [ Element.el [ Element.Font.size 24, Element.Font.italic, Element.Font.bold ]
            (Element.text "Transfer Methods")
        , Element.column
            [ Element.Border.width 1
            , Element.Border.color EH.contractBorderColor
            , Element.Border.rounded 6
            , Element.Background.color EH.contractInsetBackgroundColor
            , Element.padding 10
            , Element.spacing 5
            ]
            (paymentMethods
                |> List.map PaymentMethods.demoView
            )
        ]


claimedPhaseElement : ViewMode -> Contracts.Types.CreateParameters -> TokenValue -> Element.Element Msg
claimedPhaseElement viewMode parameters postCommitBalance =
    Element.column (phaseStyleWithViewMode Contracts.Types.Claimed viewMode)
        [ phaseHeading Contracts.Types.Claimed parameters viewMode
        , Element.column []
            [ EH.clauseList
                [ Element.paragraph []
                    [ Element.text "The "
                    , EH.seller []
                    , Element.text " is expected to verify with certainty whether he has received at least "
                    , EH.fiatValue parameters.fiatPrice
                    , Element.text " from the "
                    , EH.buyer []
                    , Element.text "."
                    , EH.clauseList
                        [ Element.paragraph []
                            [ Element.text "If the transfer has taken place, the "
                            , EH.seller []
                            , Element.text " is expected to execute the "
                            , EH.methodName "release"
                            , Element.text " method. This releases the entire balance of the contract ("
                            , EH.tokenValue postCommitBalance
                            , Element.text ") to the "
                            , EH.buyer []
                            , Element.text " and closes the contract."
                            ]
                        , Element.paragraph []
                            [ Element.text "If the transfer has not taken place, or if the transfer amount was less than "
                            , EH.fiatValue parameters.fiatPrice
                            , Element.text ", the "
                            , EH.seller []
                            , Element.text " is expected to execute the "
                            , EH.methodName "burn"
                            , Element.text " method. This burns the entire balance of the contract ("
                            , EH.tokenValue postCommitBalance
                            , Element.text ") and "
                            , EH.sectionReference "closes the contract"
                            , Element.text "."
                            ]
                        ]
                    ]
                , Element.paragraph []
                    [ Element.text "If the "
                    , EH.seller []
                    , Element.text " has not executed a "
                    , EH.methodName "burn"
                    , Element.text " or "
                    , EH.methodName "release"
                    , Element.text " within "
                    , EH.timeValue parameters.autoreleaseInterval
                    , Element.text ", "
                    , EH.methodName "release"
                    , Element.text " is triggered automatically."
                    ]
                ]
            ]
        , case viewMode of
            Draft ->
                Element.none

            Active context ->
                if context.state.phase == Contracts.Types.Claimed then
                    Element.row [ Element.spacing 50, Element.padding 20, Element.centerX ]
                        (if context.userIsSeller then
                            [ EH.contractActionButton "Release" EH.buttonGreen Release
                            , EH.contractActionButton "Burn" EH.buttonRed Burn
                            ]

                         else
                            []
                        )

                else
                    Element.none
        ]


phaseStyleWithViewMode : Contracts.Types.Phase -> ViewMode -> List (Element.Attribute a)
phaseStyleWithViewMode phase viewMode =
    case viewMode of
        Draft ->
            [ Element.width Element.fill
            ]

        Active context ->
            if context.state.phase == phase then
                [ Element.width Element.fill
                , Element.Border.width 1
                , Element.Border.color (Element.rgb 0 0 1)
                , Element.Background.color (Element.rgb 0.8 0.8 1)
                ]

            else
                [ Element.width Element.fill
                , Element.Font.color (Element.rgb 0.5 0.5 0.5)
                ]


phaseHeading : Contracts.Types.Phase -> Contracts.Types.CreateParameters -> ViewMode -> Element.Element msg
phaseHeading phase parameters viewMode =
    let
        textElement =
            Element.paragraph [ Element.Font.size 30, Element.Font.bold ]
                (case viewMode of
                    Draft ->
                        [ Element.text "During the "
                        , Element.el [ Element.Font.italic ] (Element.text (Contracts.Types.phaseToString phase ++ " phase:"))
                        ]

                    Active context ->
                        [ Element.el [ Element.Font.italic ] (Element.text (Contracts.Types.phaseToString phase ++ " phase "))
                        , Element.el [ Element.Font.size 20 ] (Element.text (phaseCountdownString phase parameters context))
                        ]
                )
    in
    Element.column [ Element.spacing 30 ]
        [ textElement
        , Element.el [] Element.none
        ]


phaseCountdownString : Contracts.Types.Phase -> Contracts.Types.CreateParameters -> ViewContext -> String
phaseCountdownString phase parameters context =
    if phase == context.state.phase then
        "("
            ++ EH.secondsRemainingString
                (TimeHelpers.add
                    context.state.phaseStartTime
                    (phaseInterval phase parameters)
                )
                context.currentTime
            ++ " left)"

    else
        ""


phaseInterval : Contracts.Types.Phase -> Contracts.Types.CreateParameters -> Time.Posix
phaseInterval phase parameters =
    case phase of
        Contracts.Types.Created ->
            Time.millisToPosix 0

        Contracts.Types.Open ->
            parameters.autorecallInterval

        Contracts.Types.Committed ->
            parameters.autoabortInterval

        Contracts.Types.Claimed ->
            parameters.autoreleaseInterval

        Contracts.Types.Closed ->
            Time.millisToPosix 0


indentedElement : Element.Element msg -> Element.Element msg
indentedElement element =
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.width (Element.px 50) ] Element.none
        , Element.el [ Element.width Element.fill ] element
        ]

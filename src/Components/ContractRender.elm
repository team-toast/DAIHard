module ContractRender exposing (ViewMode(..), render)

import Contracts.ToastytradeExtras as TTExtras
import Element
import Element.Background
import Element.Border
import Element.Font
import ElementHelpers as EH
import Eth.Types exposing (Address)
import Eth.Utils
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)



-- type PhaseStatus
--     = Draft
--     | Inactive
--     | Active
--         { userIsInitiator : Bool
--         , userIsResponder : Bool
--         }


type ViewMode
    = Draft
    | Active ViewContext


type alias ViewContext =
    { state : TTExtras.State
    , currentTime : Time.Posix
    , userAddress : Address
    }


render : ViewMode -> TTExtras.FullParameters -> Element.Element msg
render viewMode parameters =
    let
        postCommitBalance =
            TokenValue.add parameters.uncoiningAmount parameters.responderDeposit

        claimFailBurnAmount =
            TokenValue.divByInt parameters.responderDeposit 2

        totalTime =
            TimeHelpers.add parameters.autorecallInterval parameters.depositDeadlineInterval
                |> TimeHelpers.add parameters.autoreleaseInterval

        titleElement =
            Element.el
                [ Element.Font.size 42
                , Element.centerX
                ]
                (Element.text "Uncoining Contract")

        mainParametersElement =
            Element.column
                [ Element.Font.size 24
                , Element.Font.italic
                ]
                [ Element.paragraph []
                    [ EH.initiator []
                    , Element.text (": " ++ Eth.Utils.addressToString parameters.initiatorAddress)
                    ]
                , Element.paragraph []
                    [ Element.text "Trading "
                    , EH.tokenValue parameters.uncoiningAmount
                    , Element.text " for "
                    , EH.usdValue parameters.price
                    ]
                , Element.paragraph []
                    [ Element.text "to be finalized within "
                    , EH.timeValue totalTime
                    , Element.text " of contract creation."
                    ]
                ]
    in
    Element.el
        [ Element.paddingXY 150 0
        , Element.width Element.fill
        ]
        (Element.column
            [ Element.padding 60
            , Element.spacing 40
            , Element.Border.rounded 3
            , Element.Border.color (Element.rgb 0.5 0.5 0.1)
            , Element.Border.width 1
            , Element.width Element.fill
            , Element.Background.color (Element.rgb 1 1 0.7)
            , EH.contractShadowAttribute
            ]
            [ titleElement
            , mainParametersElement
            , openPhaseElement viewMode parameters
            , committedPhaseElement viewMode parameters postCommitBalance claimFailBurnAmount
            , claimedPhaseElement viewMode parameters postCommitBalance
            ]
        )


openPhaseElement : ViewMode -> TTExtras.FullParameters -> Element.Element a
openPhaseElement viewMode parameters =
    Element.column [ Element.width Element.fill ]
        [ phaseHeading viewMode "Open Phase"
        , EH.clauseList
            [ Element.paragraph []
                [ Element.text "This contract is listed on "
                , EH.fakeLink "CoinerTool browse"
                , Element.text " (and possibly other third-party tools)."
                ]
            , Element.paragraph []
                [ Element.text "Any other Ethereum user may become the "
                , EH.responder []
                , Element.text " by depositing "
                , EH.tokenValue parameters.responderDeposit
                , Element.text " into this contract. This immediately moves the contract to the "
                , EH.sectionReference "Committed Phase"
                , Element.text ". Thenceforth, the identity of the "
                , EH.responder []
                , Element.text " will never change, and the "
                , EH.initiator []
                , Element.text " will be unable to "
                , EH.methodName "recall"
                , Element.text " the trade."
                ]
            , Element.paragraph []
                [ Element.text "Until and unless an Ethereum user "
                , EH.methodName "commit"
                , Element.text "s, the "
                , EH.initiator []
                , Element.text " may execute the "
                , EH.methodName "recall"
                , Element.text " method. This "
                , EH.sectionReference "cancels the contract"
                , Element.text " and refunds the "
                , EH.initiator []
                , Element.text "'s "
                , EH.tokenValue parameters.uncoiningAmount
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
        ]


committedPhaseElement : ViewMode -> TTExtras.FullParameters -> TokenValue -> TokenValue -> Element.Element msg
committedPhaseElement viewMode parameters postCommitBalance claimFailBurnAmount =
    Element.textColumn [ Element.width Element.fill ]
        [ phaseHeading viewMode "Commited Phase"
        , indentedElement
            (Element.column []
                [ Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Committed Phase"
                    , Element.text ", the "
                    , EH.responder []
                    , Element.text " is expected to transfer "
                    , EH.usdValue parameters.uncoiningAmount
                    , Element.text " to the "
                    , EH.initiator []
                    , Element.text " via one of the "
                    , EH.sectionReference "Fiat Transfer Methods"
                    , Element.text " and mark the deposit as complete, moving the contract to the "
                    , EH.sectionReference "Claimed Phase"
                    , Element.text "."
                    ]
                , Element.paragraph [] [ Element.text " Specifically:" ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "The contract has two parties (the "
                        , EH.initiator []
                        , Element.text " and the "
                        , EH.responder []
                        , Element.text "), and has a total balance of "
                        , EH.tokenValue postCommitBalance
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.responder []
                        , Element.text " is expected to transfer "
                        , EH.usdValue parameters.uncoiningAmount
                        , Element.text " to the "
                        , EH.initiator []
                        , Element.text " as described in "
                        , EH.sectionReference "Fiat Transfer Methods"
                        , Element.text " (see below). Once the "
                        , EH.responder []
                        , Element.text " has done so, he is expected to execute the "
                        , EH.methodName "claim"
                        , Element.text " method. This will move the contract to the "
                        , EH.sectionReference "Claimed Phase"
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.initiator []
                        , Element.text " is expected to communicate any additional information the "
                        , EH.responder []
                        , Element.text " needs to complete the transfer (such as bank account numbers, contact info, etc.), either via CoinerTool's secure messaging or some other medium."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.responder []
                        , Element.text " does not claim within "
                        , EH.timeValue parameters.depositDeadlineInterval
                        , Element.text ", "
                        , EH.sectionReference "the contract is closed"
                        , Element.text ", and the balance of "
                        , EH.tokenValue postCommitBalance
                        , Element.text " is handled as follows:"
                        , EH.clauseList
                            [ Element.paragraph []
                                [ Element.text "Half of the "
                                , EH.responder []
                                , Element.text "’s deposit ("
                                , EH.tokenValue claimFailBurnAmount
                                , Element.text ") is burned and half ("
                                , EH.tokenValue claimFailBurnAmount
                                , Element.text ") is returned to the "
                                , EH.responder []
                                , Element.text "."
                                ]
                            , Element.paragraph []
                                [ Element.text "The "
                                , EH.initiator []
                                , Element.text "’s original investment of "
                                , EH.tokenValue parameters.uncoiningAmount
                                , Element.text " is refunded."
                                ]
                            ]
                        ]
                    ]
                ]
            )
        ]


claimedPhaseElement : ViewMode -> TTExtras.FullParameters -> TokenValue -> Element.Element msg
claimedPhaseElement viewMode parameters postCommitBalance =
    Element.textColumn [ Element.width Element.fill ]
        [ phaseHeading viewMode "Claimed Phase"
        , indentedElement
            (Element.column []
                [ Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Claimed Phase"
                    , Element.text ", the "
                    , EH.initiator []
                    , Element.text " is expected to "
                    , EH.methodName "release"
                    , Element.text " the contract's balance ("
                    , EH.tokenValue postCommitBalance
                    , Element.text ") to the "
                    , EH.responder []
                    , Element.text ", or if not, burn it. "
                    , Element.el [ Element.Font.bold ] (Element.text "At this point, in no case does the ")
                    , EH.initiator [ Element.Font.bold ]
                    , Element.el [ Element.Font.bold ] (Element.text " get a refund of his original investment. ")
                    , EH.fakeLink "why?"
                    ]
                , Element.paragraph [] [ Element.text "Specifically:" ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "The "
                        , EH.initiator []
                        , Element.text " is expected to verify with certainty whether the "
                        , EH.responder []
                        , Element.text " has irreversibly transferred at least "
                        , EH.usdValue parameters.uncoiningAmount
                        , Element.text " to the "
                        , EH.initiator []
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has taken place for at least "
                        , EH.usdValue parameters.uncoiningAmount
                        , Element.text ", the "
                        , EH.initiator []
                        , Element.text " is expected to execute the "
                        , EH.methodName "release"
                        , Element.text " method. This releases the entire balance of the contract ("
                        , EH.tokenValue postCommitBalance
                        , Element.text ") to the "
                        , EH.responder []
                        , Element.text " and closes the contract. Thus, the "
                        , EH.responder []
                        , Element.text " will have made a profit of ??? by servicing this Uncoining contract."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has not taken place, or if the transfer amount was less than "
                        , EH.usdValue parameters.uncoiningAmount
                        , Element.text ", the "
                        , EH.initiator []
                        , Element.text " is expected to execute the "
                        , EH.methodName "burn"
                        , Element.text " method. This burns the entire balance of the contract ("
                        , EH.tokenValue postCommitBalance
                        , Element.text ") and "
                        , EH.sectionReference "closes the contract"
                        , Element.text ". Thus, the "
                        , EH.responder []
                        , Element.text " will have suffered a loss of "
                        , EH.tokenValue parameters.responderDeposit
                        , Element.text " for failing to make the deposit."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.initiator []
                        , Element.text " is not sure whether the transfer has taken place, the "
                        , EH.initiator []
                        , Element.text " and the "
                        , EH.responder []
                        , Element.text " are expected to communicate to resolve any ambiguity."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.initiator []
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
            )
        ]


phaseHeading : ViewMode -> String -> Element.Element msg
phaseHeading viewMode phaseName =
    let
        textElement =
            Element.paragraph [ Element.Font.size 30, Element.Font.bold ]
                (case viewMode of
                    Draft ->
                        [ Element.text "During the "
                        , Element.el [ Element.Font.italic ] (Element.text (phaseName ++ ":"))
                        ]

                    Active _ ->
                        [ Element.el [ Element.Font.italic ] (Element.text phaseName)
                        ]
                )
    in
    Element.column [ Element.spacing 30 ]
        [ textElement
        , Element.el [] Element.none
        ]


indentedElement : Element.Element msg -> Element.Element msg
indentedElement element =
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.width (Element.px 50) ] Element.none
        , Element.el [ Element.width Element.fill ] element
        ]

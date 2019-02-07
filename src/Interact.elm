module Interact exposing (Model, Msg(..), init, update, updateWithUserAddress, view)

import ChainCmd exposing (ChainCmdOrder)
import ContractRender
import Contracts.ToastytradeExtras as TTExtras
import Contracts.ToastytradeSell as TTS
import Element
import Element.Background
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Http
import Time


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userAddress : Maybe Address
    , tokenAddress : Address
    , tokenDecimals : Int
    , addressInput : String
    , ttsInfo : TTSInfo
    }


type alias TTSInfo =
    { address : Maybe Address
    , parameters : Maybe TTExtras.FullParameters
    , state : Maybe TTExtras.State
    }


updateParameters : TTSInfo -> Maybe TTExtras.FullParameters -> TTSInfo
updateParameters ttsInfo parameters =
    { ttsInfo | parameters = parameters }


updateState : TTSInfo -> Maybe TTExtras.State -> TTSInfo
updateState ttsInfo state =
    { ttsInfo | state = state }


init : EthHelpers.EthNode -> Address -> Int -> Maybe Address -> Maybe Address -> ( Model, Cmd Msg, ChainCmdOrder Msg )
init ethNode tokenAddress tokenDecimals userAddress maybeTTAddress =
    let
        cmd =
            case maybeTTAddress of
                Just address ->
                    getContractInfoCmd ethNode tokenDecimals address

                Nothing ->
                    Cmd.none
    in
    ( { ethNode = ethNode
      , userAddress = userAddress
      , tokenAddress = tokenAddress
      , tokenDecimals = tokenDecimals
      , addressInput =
            Maybe.map Eth.Utils.addressToString maybeTTAddress
                |> Maybe.withDefault ""
      , ttsInfo =
            { address = Nothing
            , parameters = Nothing
            , state = Nothing
            }
      }
    , cmd
    , ChainCmd.none
    )


getContractInfoCmd : EthHelpers.EthNode -> Int -> Address -> Cmd Msg
getContractInfoCmd ethNode tokenDecimals address =
    Cmd.batch
        [ TTExtras.getStateCmd ethNode tokenDecimals address StateFetched
        , TTExtras.getParametersCmd ethNode tokenDecimals address ParametersFetched
        ]


type Msg
    = AddressInputChanged String
    | StateFetched (Result Http.Error (Maybe TTExtras.State))
    | ParametersFetched (Result Http.Error (Maybe TTExtras.FullParameters))
    | ContractAction ContractRender.Msg


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmdOrder Msg )
update msg model =
    case msg of
        AddressInputChanged newInput ->
            let
                ttsInfo =
                    { address = Result.toMaybe (Eth.Utils.toAddress newInput)
                    , parameters = Nothing
                    , state = Nothing
                    }
            in
            ( { model
                | addressInput = newInput
                , ttsInfo = ttsInfo
              }
            , case ttsInfo.address of
                Just address ->
                    getContractInfoCmd model.ethNode model.tokenDecimals address

                Nothing ->
                    Cmd.none
            , ChainCmd.none
            )

        StateFetched fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    ( { model | ttsInfo = updateState model.ttsInfo (Just state) }, Cmd.none, ChainCmd.none )

                _ ->
                    handleBadFetchResult model fetchResult

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    ( { model | ttsInfo = updateParameters model.ttsInfo (Just parameters) }, Cmd.none, ChainCmd.none )

                _ ->
                    handleBadFetchResult model fetchResult

        ContractAction actionMsg ->
            let
                _ =
                    Debug.log "button clicked" actionMsg
            in
            ( model, Cmd.none, ChainCmd.none )


handleBadFetchResult : Model -> Result Http.Error (Maybe a) -> ( Model, Cmd Msg, ChainCmdOrder Msg )
handleBadFetchResult model fetchResult =
    case fetchResult of
        Ok (Just a) ->
            let
                _ =
                    Debug.log "I'm confused about whether this is a bad fetch result or not!."
            in
            ( model, Cmd.none, ChainCmd.none )

        Ok Nothing ->
            let
                _ =
                    Debug.log "The data was fetched, but could not be decoded."
            in
            ( model, Cmd.none, ChainCmd.none )

        Err errstr ->
            let
                _ =
                    Debug.log "can't fetch full state: " errstr
            in
            ( model, Cmd.none, ChainCmd.none )


view : Model -> Element.Element Msg
view model =
    Element.column [ Element.spacing 40, Element.width Element.fill ]
        [ addressInputFormElement model
        , maybeContractElement model
        ]


addressInputFormElement : Model -> Element.Element Msg
addressInputFormElement model =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Element.centerX, Element.Font.size 16 ] (Element.text "Uncoining Contract at:")
        , Element.Input.text [ Element.centerX, Element.width (Element.px 430), Element.Font.size 16 ]
            { onChange = AddressInputChanged
            , text = model.addressInput
            , placeholder = Just (Element.Input.placeholder [] (Element.text "contract address"))
            , label = Element.Input.labelHidden "address"
            }
        ]


maybeContractElement : Model -> Element.Element Msg
maybeContractElement model =
    case ( model.userAddress, model.ttsInfo.parameters, model.ttsInfo.state ) of
        ( Just userAddress, Just parameters, Just state ) ->
            let
                context =
                    { state = state
                    , currentTime = Time.millisToPosix 0
                    , userIsInitiator = userAddress == parameters.initiatorAddress
                    , userIsResponder =
                        case state.responder of
                            Just responderAddress ->
                                userAddress == responderAddress

                            Nothing ->
                                False
                    }
            in
            Element.map ContractAction (ContractRender.render (ContractRender.Active context) parameters)

        ( Nothing, _, _ ) ->
            Element.text "Can't find user address!"

        ( _, Nothing, _ ) ->
            Element.text "Don't have contract parameters!"

        ( _, _, Nothing ) ->
            Element.text "Don't have contract state!"

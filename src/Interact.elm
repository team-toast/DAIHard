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


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userAddress : Maybe Address
    , tokenAddress : Address
    , tokenDecimals : Int
    , addressInput : String
    , ttsInfo : Maybe TTSInfo
    }


type alias TTSInfo =
    { address : Address
    , parameters : TTExtras.FullParameters
    , state : TTExtras.State
    }


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
      , ttsInfo = Nothing
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


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmdOrder Msg )
update msg model =
    case msg of
        AddressInputChanged newInput ->
            let
                cmd =
                    case Eth.Utils.toAddress newInput of
                        Ok address ->
                            getContractInfoCmd model.ethNode model.tokenDecimals address

                        Err _ ->
                            Cmd.none
            in
            ( { model | addressInput = newInput }
            , cmd
            , ChainCmd.none
            )

        StateFetched fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    let
                        _ =
                            Debug.log "full state" state
                    in
                    ( model, Cmd.none, ChainCmd.none )

                _ ->
                    handleBadFetchResult model fetchResult

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    let
                        _ =
                            Debug.log "parameters" parameters
                    in
                    ( model, Cmd.none, ChainCmd.none )

                _ ->
                    handleBadFetchResult model fetchResult


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
        ]


addressInputFormElement : Model -> Element.Element Msg
addressInputFormElement model =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Element.centerX, Element.Font.size 16 ] (Element.text "Uncoining Contract at:")
        , Element.Input.text [ Element.centerX, Element.width (Element.px 400) ]
            { onChange = AddressInputChanged
            , text = model.addressInput
            , placeholder = Just (Element.Input.placeholder [] (Element.text "contract address"))
            , label = Element.Input.labelHidden "address"
            }
        ]

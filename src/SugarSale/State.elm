module SugarSale.State exposing (init, runCmdDown, subscriptions, update)

import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Eth
import Helpers.Eth as EthHelpers
import SugarSale.Types exposing (..)
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Utils
import Wallet


init : Bool -> Wallet.State -> ( Model, Cmd Msg )
init testMode wallet =
    ( { wallet = wallet
      , testMode = testMode
      , currentBlock = Debug.todo ""
      , buckets = Debug.todo ""
      }
    , fetchBlocknumCmd
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        CmdUp cmdUp ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ cmdUp ]

        Refresh ->
            UpdateResult
                prevModel
                fetchBlocknumCmd
                ChainCmd.none
                []

        BlocknumFetched fetchResult ->
            case fetchResult of
                Ok blocknum ->
                    justModelUpdate
                        { prevModel | currentBlock = Just blocknum }

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching blocknum" httpErr
                    in
                    justModelUpdate prevModel


fetchBlocknumCmd : Cmd Msg
fetchBlocknumCmd =
    Eth.getBlockNumber (EthHelpers.httpProviderForFactory (Token EthDai))
        |> Task.attempt BlocknumFetched


runCmdDown : CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            UpdateResult
                { prevModel | wallet = wallet }
                Cmd.none
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 500 (always Refresh)

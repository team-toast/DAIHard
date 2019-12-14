module SugarSale.State exposing (init, runCmdDown, subscriptions, update)

import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import SugarSale.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Utils
import Wallet


init : Bool -> Wallet.State -> UpdateResult
init testMode wallet =
    Debug.todo ""


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        _ ->
            Debug.todo ""


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
    Sub.batch
        [ Time.every 500 UpdateNow
        ]

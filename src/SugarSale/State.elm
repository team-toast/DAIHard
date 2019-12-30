module SugarSale.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.SugarSale.Wrappers as SugarSaleContract
import Eth
import Eth.Types exposing (HttpProvider)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import List.Extra
import SugarSale.Types exposing (..)
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Utils
import Wallet


init : Bool -> Wallet.State -> ( Model, Cmd Msg )
init testMode wallet =
    if testMode then
        ( { wallet = wallet
          , testMode = testMode
          , currentBlock = Nothing
          , saleStartblock = Nothing
          , sugarSale = Nothing
          , bucketView = ViewActive
          }
        , Cmd.batch
            [ fetchBlocknumCmd testMode
            , fetchSaleStartblockCmd testMode
            ]
        )

    else
        Debug.todo "must use test mode"


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
                (fetchBlocknumCmd prevModel.testMode)
                ChainCmd.none
                []

        BlocknumFetched fetchResult ->
            case fetchResult of
                Ok blocknum ->
                    let
                        newMaybeSugarSale =
                            case ( prevModel.saleStartblock, prevModel.sugarSale ) of
                                ( Just saleStartblock, Nothing ) ->
                                    case initSugarSale prevModel.testMode saleStartblock blocknum of
                                        Just s ->
                                            Just s

                                        Nothing ->
                                            Debug.todo "Failed to init sugar sale. Is it started yet?"

                                ( Just _, Just sugarSale ) ->
                                    Just
                                        (sugarSale
                                            |> addNewActiveBucketIfNeeded blocknum prevModel.testMode
                                        )

                                ( Nothing, _ ) ->
                                    prevModel.sugarSale
                    in
                    justModelUpdate
                        { prevModel
                            | currentBlock = Just blocknum
                            , sugarSale = newMaybeSugarSale
                        }

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching blocknum" httpErr
                    in
                    justModelUpdate prevModel

        SaleStartblockFetched fetchResult ->
            case fetchResult of
                Ok startblockBigInt ->
                    let
                        startblock =
                            BigIntHelpers.toIntWithWarning startblockBigInt

                        newMaybeSugarSale =
                            case ( prevModel.currentBlock, prevModel.sugarSale ) of
                                ( Just currentBlock, Nothing ) ->
                                    case initSugarSale prevModel.testMode startblock currentBlock of
                                        Just s ->
                                            Just s

                                        Nothing ->
                                            Debug.todo "Failed to init sugar sale. Is it started yet?"

                                _ ->
                                    prevModel.sugarSale
                    in
                    justModelUpdate
                        { prevModel
                            | sugarSale = newMaybeSugarSale
                            , saleStartblock = Just startblock
                        }

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching sale startblock" httpErr
                    in
                    justModelUpdate prevModel


initSugarSale : Bool -> Int -> Int -> Maybe SugarSale
initSugarSale testMode saleStartBlock currentBlock =
    if saleStartBlock == 0 then
        Nothing

    else
        let
            blocksPerBucket =
                Config.sugarSaleBlocksPerBucket testMode

            allBuckets =
                List.Extra.iterate
                    (\lastBucketAdded ->
                        let
                            nextBucketStartblock =
                                lastBucketAdded.startBlock + blocksPerBucket
                        in
                        if nextBucketStartblock > currentBlock then
                            Nothing

                        else
                            Just <|
                                Bucket
                                    nextBucketStartblock
                                    Nothing
                    )
                    (Bucket
                        saleStartBlock
                        Nothing
                    )
        in
        Maybe.map2
            (SugarSale saleStartBlock)
            (List.Extra.init allBuckets)
            (List.Extra.last allBuckets)


addNewActiveBucketIfNeeded : Int -> Bool -> SugarSale -> SugarSale
addNewActiveBucketIfNeeded currentBlock testMode prevSugarSale =
    let
        nextBucketStartblock =
            prevSugarSale.activeBucket.startBlock + Config.sugarSaleBlocksPerBucket testMode
    in
    if nextBucketStartblock <= currentBlock then
        { prevSugarSale
            | pastBuckets =
                List.append
                    prevSugarSale.pastBuckets
                    [ prevSugarSale.activeBucket ]
            , activeBucket =
                Bucket
                    nextBucketStartblock
                    Nothing
        }

    else
        prevSugarSale


httpProvider : Bool -> HttpProvider
httpProvider test =
    if test then
        EthHelpers.httpProviderForFactory <| Token KovanDai

    else
        Debug.todo "non-test mode not yet defined for httpProvider"


fetchBlocknumCmd : Bool -> Cmd Msg
fetchBlocknumCmd test =
    Eth.getBlockNumber (httpProvider test)
        |> Task.attempt BlocknumFetched


fetchSaleStartblockCmd : Bool -> Cmd Msg
fetchSaleStartblockCmd test =
    SugarSaleContract.getSaleStartBlockCmd
        (httpProvider test)
        SaleStartblockFetched


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
    Time.every 1000 (always Refresh)

module ConstructorTests

open FsUnit.Xunit
open Xunit
open Nethereum.RPC.Eth.DTOs
open Nethereum.Contracts
open TestBase
open System.Numerics
open System.Linq
open Nethereum.Hex.HexConvertors.Extensions

let DAI =
    let abi = Abi("../../../../build/contracts/TestToken.json")

    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| "MCD DAI stable coin"
               "DAI"
               ethConn.Account.Address
               BigInteger(1000000UL) |]
        |> runNow

    let result = ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)
    result.QueryFunction "balanceOf" [| ethConn.Account.Address |]
    |> should equal (BigInteger(1000000UL) * BigInteger(1000000000000000000UL))
    result


let FRY =
    let abi = Abi("../../../../build/contracts/TestToken.json")

    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| "Foundry logistics token"
               "FRY"
               ethConn.Account.Address
               BigInteger(1000000UL) |]
        |> runNow

    let result = ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)
    result


let bucketSale =
    let abi = Abi("../../../../build/contracts/BucketSale.json")

    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| ethConn.Account.Address; startOfSale; bucketPeriod; bucketSupply; bucketCount; DAI.Address; FRY.Address |]
        |> runNow

    let transferFryData =
        FRY.FunctionData "transfer"
            [| DAI.Address
               bucketSupply * bucketCount |]
    let transferFryTxReceipt = ethConn.SendTxAsync DAI.Address transferFryData (BigInteger(0UL)) |> runNow
    transferFryTxReceipt |> shouldSucceed

    ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)


[<Specification("BucketSale", "misc", 0)>]
[<Fact>]
let ``Can send eth``() =
    let balance = ethConn.Web3.Eth.GetBalance.SendRequestAsync(ethConn.Account.Address) |> runNow
    balance.Value |> should greaterThan (bigInt 0UL)

    let transactionInput =
        TransactionInput
            ("", zeroAddress, ethConn.Account.Address, hexBigInt 4000000UL, hexBigInt 1000000000UL, hexBigInt 1UL)

    let sendEthTxReceipt =
        ethConn.Web3.Eth.TransactionManager.SendTransactionAndWaitForReceiptAsync(transactionInput, null) |> runNow

    sendEthTxReceipt |> shouldSucceed

    let balanceAfter = ethConn.Web3.Eth.GetBalance.SendRequestAsync(zeroAddress) |> runNow
    balanceAfter.Value |> should greaterThan (bigInt 1UL)

[<Specification("BucketSale", "constructor", 1)>]
[<Fact>]
let ``Can construct the contract``() =
    let abi = Abi("../../../../build/contracts/BucketSale.json")
    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| ethConn.Account.Address; startOfSale; bucketPeriod; bucketSupply; bucketCount; zeroAddress; zeroAddress |]
        |> runNow

    deployTxReceipt |> shouldSucceed

    // Assert
    let contract = ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

    contract.QueryFunction "owner" [||] |> shouldEqualIgnoringCase ethConn.Account.Address
    contract.QueryFunction "startOfSale" [||] |> should equal startOfSale
    contract.QueryFunction "bucketPeriod" [||] |> should equal bucketPeriod
    contract.QueryFunction "bucketSupply" [||] |> should equal bucketSupply
    contract.QueryFunction "bucketCount" [||] |> should equal bucketCount
    contract.QueryFunction "tokenOnSale" [||] |> shouldEqualIgnoringCase tokenOnSale
    contract.QueryFunction "tokenSoldFor" [||] |> shouldEqualIgnoringCase tokenSoldFor


let shouldRevertWithMessage msg (forwardedEvent: ForwardedEvent) =
    match forwardedEvent.ResultAsRevertMessage with
    | None -> failwith "not a revert message"
    | Some msg -> msg |> should haveSubstring msg


[<Specification("BucketSale", "enter", 1)>]
[<Fact>]
let ``Cannot enter bucket sale without putting some money down``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "can't buy nothing"


[<Specification("BucketSale", "enter", 2)>]
// [<Theory>]
// [<InlineData(1)>]
// [<InlineData(2)>]
// [<InlineData(3)>]
[<Fact>]
let ``Cannot enter a past bucket``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||] |> uint64
    let incorrectBucket = currentBucket - 1UL
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; incorrectBucket; 1UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "cannot enter past buckets"


[<Specification("BucketSale", "enter", 2)>]
[<Fact>]
let ``Cannot enter bucket sale if there are not enough tokens to payout``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]

    let moveDaiData = DAI.FunctionData "transfer" [| zeroAddress; 1UL |]
    let moveDaiForwardData =
        bucketSale.FunctionData "forward"
            [| DAI.Address
               moveDaiData.HexToByteArray()
               BigInteger(0UL) |]
    let moveDaiTxReceipt = ethConn.SendTxAsync bucketSale.Address moveDaiForwardData (BigInteger(0)) |> runNow
    moveDaiTxReceipt |> shouldSucceed

    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "insufficient tokens to sell"


[<Specification("BucketSale", "enter", 3)>]
[<Specification("BucketSale", "enter", 7)>]
[<Fact>]
let ``Cannot enter bucket sale with 0 amount``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "can't buy nothing"


[<Specification("BucketSale", "enter", 4)>]
[<Fact>]
let ``Cannot enter a bucket after the designated bucket count if there is no referrer``() =
    let bucketCount = bucketSale.QueryFunction "bucketCount" [||] // will be one bucket beyond what is allowed
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; bucketCount; 1UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "invalid bucket id--past end of sale"

[<Specification("BucketSale", "enter", 5)>]
[<Specification("BucketSale", "enter", 9)>]
[<Fact>]
let ``Cannot enter a bucket if payment reverts``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||] // will be one bucket beyond what is allowed
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "insufficient tokens to sell"

module ConstructorTests

open FsUnit.Xunit
open Xunit
open Nethereum.RPC.Eth.DTOs
open Nethereum.Contracts
open TestBase
open System.Numerics
open System.Linq
open Nethereum.Hex.HexConvertors.Extensions
open Constants
open DTOs

let DAI =
    let abi = Abi("../../../../build/contracts/TestToken.json")

    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| "MCD DAI stable coin"
               "DAI"
               ethConn.Account.Address
               bucketSupply * bucketCount * BigInteger(100UL) |]
        |> runNow

    let result = ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)
    result.QueryFunction "balanceOf" [| ethConn.Account.Address |]
    |> should equal (bucketSupply * bucketCount * BigInteger(100UL) * BigInteger(1000000000000000000UL))
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
            [| ethConn.Account.Address; startOfSale; bucketPeriod; bucketSupply; bucketCount; FRY.Address; DAI.Address |]
        |> runNow

    ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

let seedBucketWithFries() =
    let transferFryTxReceipt =
        FRY.ExecuteFunction "transfer"
            [| bucketSale.Address
               bucketSupply * bucketCount |]
    transferFryTxReceipt |> shouldSucceed
    FRY.QueryFunction "balanceOf" [| bucketSale.Address |] |> should equal (bucketSupply * bucketCount)

let seedWithDAI (recipient:string) (amount:BigInteger) =
    let balanceBefore = DAI.QueryFunction "balanceOf" [| recipient |] 
    let transferDaiTxReceipt =
        DAI.ExecuteFunction "transfer"
            [| recipient
               bucketSupply * bucketCount |]
    transferDaiTxReceipt |> shouldSucceed
    DAI.QueryFunction "balanceOf" [| recipient |] |> should equal (balanceBefore + amount)


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


[<Specification("BucketSale", "enter", 1)>]
[<Fact>]
let ``Cannot enter bucket sale without putting some money down``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

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

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

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

    let moveDaiTxReceipt =
        moveDaiForwardData
        |> ethConn.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow
    moveDaiTxReceipt |> shouldSucceed

    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "insufficient tokens to sell"


[<Specification("BucketSale", "enter", 3)>]
[<Specification("BucketSale", "enter", 7)>]
[<Fact>]
let ``Cannot enter bucket sale with 0 amount``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "can't buy nothing"


[<Specification("BucketSale", "enter", 4)>]
[<Fact>]
let ``Cannot enter a bucket after the designated bucket count if there is no referrer``() =
    let bucketCount = bucketSale.QueryFunction "bucketCount" [||] // will be one bucket beyond what is allowed
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; bucketCount; 1UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "invalid bucket id--past end of sale"


[<Specification("BucketSale", "enter", 5)>]
[<Specification("BucketSale", "enter", 9)>]
[<Fact>]
let ``Cannot enter a bucket if payment reverts``() =
    seedBucketWithFries |> ignore
    seedWithDAI forwarder.ContractPlug.Address (BigInteger(10UL))
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||] // will be one bucket beyond what is allowed
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "insufficient tokens to sell"

    
[<Specification("BucketSale", "enter", 6)>]
[<Fact>]
let ``Can enter a bucket with no referrer``() =
    // arrange
    seedBucketWithFries |> ignore

    let valueToEnter = BigInteger(10UL)
    let approveDaiTxReceipt = DAI.ExecuteFunction "approve" [| bucketSale.Address; valueToEnter |]
    approveDaiTxReceipt |> shouldSucceed

    let currentBucket = bucketSale.QueryFunction "currentBucket" [||] // will be one bucket beyond what is allowed

    // act
    let receipt =
        bucketSale.ExecuteFunction "enter" [| ethConn.Account.Address; currentBucket; valueToEnter; zeroAddress |]

    // assert
    receipt |> shouldSucceed
    // event validation
    let enteredEvent = receipt |> decodeFirstEvent<EnteredEvent>
    enteredEvent.BucketId |> should equal currentBucket
    enteredEvent.Buyer |> should equal ethConn.Account.Address
    enteredEvent.BuyerReferralReward |> should equal (BigInteger(0UL))
    enteredEvent.MsgSender |> should equal ethConn.Account.Address
    enteredEvent.Referrer |> should equal zeroAddress
    enteredEvent.ReferrerReferralReward |> should equal (BigInteger(0UL))
    enteredEvent.ValueEntered |> should equal (BigInteger(1UL))
    // state validation
    // unchanged state
    bucketSale.QueryFunction "owner" [||] |> shouldEqualIgnoringCase ethConn.Account.Address
    bucketSale.QueryFunction "startOfSale" [||] |> should equal startOfSale
    bucketSale.QueryFunction "bucketPeriod" [||] |> should equal bucketPeriod
    bucketSale.QueryFunction "bucketSupply" [||] |> should equal bucketSupply
    bucketSale.QueryFunction "bucketCount" [||] |> should equal bucketCount
    bucketSale.QueryFunction "tokenOnSale" [||] |> shouldEqualIgnoringCase tokenOnSale
    bucketSale.QueryFunction "tokenSoldFor" [||] |> shouldEqualIgnoringCase tokenSoldFor
    // changed state
    bucketSale.QueryFunction "buckets" [| currentBucket |] |> printf "%A"

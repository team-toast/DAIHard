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
open System
open DAIHard.Contracts.BucketSale.ContractDefinition
open DAIHard.Contracts.TestToken.ContractDefinition
open DAIHard.Contracts.TestToken

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
    result.Query "balanceOf" [| ethConn.Account.Address |] 
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
    let frySupplyBefore = FRY.Query "balanceOf" [| bucketSale.Address |]
    let transferFryTxReceipt =
        FRY.ExecuteFunction "transfer"
            [| bucketSale.Address
               bucketSupply * bucketCount |]
    transferFryTxReceipt |> shouldSucceed
    FRY.Query "balanceOf" [| bucketSale.Address |] |> should equal (frySupplyBefore + bucketSupply * bucketCount)

let seedWithDAI (recipient:string) (amount:BigInteger) =
    let balanceBefore = DAI.Query "balanceOf" [| recipient |] 
    let transferDaiTxReceipt = DAI.ExecuteFunction "transfer" [| recipient; amount |]
    transferDaiTxReceipt |> shouldSucceed
    DAI.Query "balanceOf" [| recipient |] |> should equal (balanceBefore + amount)


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

    contract.Query "owner" [||] |> shouldEqualIgnoringCase ethConn.Account.Address
    contract.Query "startOfSale" [||] |> should equal startOfSale
    contract.Query "bucketPeriod" [||] |> should equal bucketPeriod
    contract.Query "bucketSupply" [||] |> should equal bucketSupply
    contract.Query "bucketCount" [||] |> should equal bucketCount
    contract.Query "tokenOnSale" [||] |> shouldEqualIgnoringCase tokenOnSale
    contract.Query "tokenSoldFor" [||] |> shouldEqualIgnoringCase tokenSoldFor


[<Specification("BucketSale", "enter", 1)>]
[<Fact>]
let ``Cannot enter bucket sale without putting some money down``() =
    let currentBucket = bucketSale.Query "currentBucket" [||]
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
    let currentBucket = bucketSale.Query "currentBucket" [||] |> uint64
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
    let currentBucket = bucketSale.Query "currentBucket" [||]

    let moveDaiData = DAI.FunctionData "transfer" [| zeroAddress; 1UL |]
    let moveDaiForwardData =
        bucketSale.FunctionData "forward"
            [| DAI.Address
               moveDaiData.HexToByteArray()
               BigInteger.Zero |]

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
    let currentBucket = bucketSale.Query "currentBucket" [||]
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
    let bucketCount = bucketSale.Query "bucketCount" [||] // will be one bucket beyond what is allowed
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
    seedBucketWithFries()
    seedWithDAI forwarder.ContractPlug.Address (BigInteger(10UL))
    let currentBucket = bucketSale.Query "currentBucket" [||] // will be one bucket beyond what is allowed
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]

    let receipt =
        data
        |> forwarder.SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage ""


let enterBucket sender buyer bucketToEnter valueToEnter referrer =
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let approveDaiTxReceipt = DAI.ExecuteFunction "approve" [| bucketSale.Address; valueToEnter |]
    approveDaiTxReceipt |> shouldSucceed

    let referredTotalBefore = bucketSale.Query "referredTotal" [| referrer |]
    let senderDaiBalanceBefore = DAI.Query "balanceOf" [| sender |]
    let bucketDaiBalanceBefore = DAI.Query "balanceOf" [| bucketSale.Address |]
    let buyForBucketBefore = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter; buyer |]
    let bucketBefore = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter |] 

    // act
    let receipt = bucketSale.ExecuteFunction "enter" [| buyer; bucketToEnter; valueToEnter; referrer |]

    // assert
    receipt |> shouldSucceed

    // event validation
    let enteredEvent = receipt |> decodeFirstEvent<EnteredEventDTO>
    enteredEvent.BucketId |> should equal bucketToEnter
    enteredEvent.Buyer |> shouldEqualIgnoringCase buyer
    enteredEvent.BuyerReferralReward |> should equal (BigInteger.Zero)
    enteredEvent.Sender |> shouldEqualIgnoringCase sender
    enteredEvent.Referrer |> shouldEqualIgnoringCase referrer
    enteredEvent.ReferrerReferralReward |> should equal (BigInteger.Zero)
    enteredEvent.ValueEntered |> should equal valueToEnter

    // state validation
    // unchanged state
    bucketSale.Query "owner" [||] |> shouldEqualIgnoringCase ethConn.Account.Address
    bucketSale.Query "startOfSale" [||] |> should equal startOfSale
    bucketSale.Query "bucketPeriod" [||] |> should equal bucketPeriod
    bucketSale.Query "bucketSupply" [||] |> should equal bucketSupply
    bucketSale.Query "bucketCount" [||] |> should equal bucketCount
    bucketSale.Query "tokenOnSale" [||] |> shouldEqualIgnoringCase FRY.Address
    bucketSale.Query "tokenSoldFor" [||] |> shouldEqualIgnoringCase DAI.Address

    // changed state
    let referredTotalAfter = bucketSale.Query "referredTotal" [| referrer |]
    referredTotalAfter |> should equal (referredTotalBefore + valueToEnter)

    let senderDaiBalanceAfter = DAI.Query "balanceOf" [| sender |]
    senderDaiBalanceAfter |> should equal (senderDaiBalanceBefore - valueToEnter) 
    let bucketDaiBalanceAfter = DAI.Query "balanceOf" [| bucketSale.Address |]
    bucketDaiBalanceAfter |> should equal (bucketDaiBalanceBefore + valueToEnter)

    let buyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter; buyer |] 
    buyForBucketAfter.ValueEntered |> should equal (buyForBucketBefore.ValueEntered + valueToEnter)
    buyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

    let bucketAfter = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter |]
    bucketAfter.TotalValueEntered |> should equal (bucketBefore.TotalValueEntered + valueToEnter)

[<Specification("BucketSale", "enter", 6)>]
[<Fact>]
let ``Can enter a bucket with no referrer``() =
    // arrange
    seedBucketWithFries()

    let currentBucket:BigInteger = bucketSale.Query "currentBucket" [||] // will be one bucket beyond what is allowed
    
    let valueToEnter = BigInteger 10UL
    let referrer = zeroAddress
    let buyer = ethConn.Account.Address
    let sender = ethConn.Account.Address

    let bucketsToEnter = 
        rndRange (currentBucket |> int) (bucketCount - BigInteger.One |> int) 
        |> Seq.take 5
        |> Seq.toArray
        |> Array.append [| currentBucket; bucketCount - BigInteger.One |]

    Array.ForEach(
        bucketsToEnter, 
        fun bucketToEnter -> enterBucket sender buyer bucketToEnter valueToEnter referrer)
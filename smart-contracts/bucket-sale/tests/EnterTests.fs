module EnterTests

open FsUnit.Xunit
open Xunit
open TestBase
open System.Numerics
open Nethereum.Hex.HexConvertors.Extensions
open Constants
open System
open DAIHard.Contracts.BucketSale.ContractDefinition
open BucketSaleTestBase
open Nethereum.Web3.Accounts
open Nethereum.RPC.Eth.DTOs
open Nethereum.Contracts
open System.Text

[<Specification("BucketSale", "misc", 0)>]
[<Fact>]
let ``M00 - Can send eth``() =
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
let ``C00 - Can construct the contract``() =
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
let ``E001 - Cannot enter bucket sale without putting some money down``() =
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]

    let receipt =
        data
        |> (forwarder :> IAsyncTxSender).SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "can't buy nothing"

[<Specification("BucketSale", "enter", 2)>]
[<Fact>]
let ``E002 - Cannot enter bucket sale if there are not enough tokens to payout``() =
    let currentBucket = bucketSale.Query "currentBucket" [||]

    let balanceBeforeReduction = FRY.Query "balanceOf" [| bucketSale.Address |]
    let moveFryData = FRY.FunctionData "transfer" [| zeroAddress; balanceBeforeReduction |]
    let moveFryForwardData =
        bucketSale.FunctionData "forward"
            [| FRY.Address
               moveFryData.HexToByteArray()
               BigInteger.Zero |]
    let moveFryTxReceipt =
        moveFryForwardData
        |> (ethConn :> IAsyncTxSender).SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow
    moveFryTxReceipt |> shouldSucceed

    let balanceBeforeEntry = FRY.Query "balanceOf" [| bucketSale.Address |]
    balanceBeforeEntry |> should equal BigInteger.Zero
    balanceBeforeEntry |> should lessThan (bucketCount * bucketSupply)

    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |]

    let receipt =
        data
        |> (forwarder :> IAsyncTxSender).SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "insufficient tokens to sell"


[<Specification("BucketSale", "enter", 3)>]
[<Specification("BucketSale", "enter", 7)>]
[<Fact>]
let ``E003|E007 - Cannot enter a past bucket``() =
    let currentBucket = bucketSale.Query "currentBucket" [||] |> uint64
    let incorrectBucket = currentBucket - 1UL
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; incorrectBucket; 1UL; zeroAddress |]

    let receipt =
        data
        |> (forwarder :> IAsyncTxSender).SendTxAsync bucketSale.Address (BigInteger(0))
        |> runNow

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "cannot enter past buckets"


[<Specification("BucketSale", "enter", 4)>]
[<Fact>]
let ``E004 - Cannot enter a bucket beyond the designated bucket count (no referrer)``() =
    seedBucketWithFries()
    let bucketCount = bucketSale.Query "bucketCount" [||] // will be one bucket beyond what is allowed
    let receipt = bucketSale.ExecuteFunctionFrom "enter" [| ethConn.Account.Address; bucketCount; 1UL; zeroAddress |] forwarder
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "invalid bucket id--past end of sale"


[<Specification("BucketSale", "enter", 5)>]
[<Fact>]
let ``E005 - Cannot enter a bucket if payment reverts (with no referrer)``() =
    seedBucketWithFries()
    seedWithDAI forwarder.ContractPlug.Address (BigInteger(10UL))
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let receipt = bucketSale.ExecuteFunctionFrom "enter" [| ethConn.Account.Address; currentBucket; 1UL; zeroAddress |] forwarder
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage ""


let referrerReward amount =
    ((amount / BigInteger 1000000000000000000UL) + BigInteger 10000UL)

let enterBucket sender buyer bucketToEnter valueToEnter referrer =
    let approveDaiTxReceipt = DAI.ExecuteFunction "approve" [| bucketSale.Address; valueToEnter |]
    approveDaiTxReceipt |> shouldSucceed

    let currentBucket = bucketSale.Query "currentBucket" [||]
    let referredTotalBefore = bucketSale.Query "referredTotal" [| referrer |]
    let referrerRewardPercBefore = bucketSale.Query "referrerReferralRewardPerc" [| referrer |]
    let calculatedReferrerRewardPercBefore = referrerReward referredTotalBefore
    referrerRewardPercBefore |> should equal calculatedReferrerRewardPercBefore
    let senderDaiBalanceBefore = DAI.Query "balanceOf" [| sender |]
    let bucketDaiBalanceBefore = DAI.Query "balanceOf" [| bucketSale.Address |]
    let buyForBucketBefore = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter; buyer |]
    let buyerRewardBuyForBucketBefore = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; buyer |]
    let referrerRewardBuyForBucketBefore = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; referrer |]
    let bucketBefore = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter |]
    let referralBucketBefore = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter + BigInteger.One |]

    // act
    let receipt = bucketSale.ExecuteFunction "enter" [| buyer; bucketToEnter; valueToEnter; referrer |]

    // assert
    receipt |> shouldSucceed

    // event validation
    let referredTotalAfter = bucketSale.Query "referredTotal" [| referrer |]
    referredTotalAfter |> should equal (referredTotalBefore + valueToEnter)
    let calculatedReferrerRewardPercAfter = referrerReward referredTotalAfter
    let referrerRewardPercAfter = bucketSale.Query "referrerReferralRewardPerc" [| referrer |]
    referrerRewardPercAfter |> should equal calculatedReferrerRewardPercAfter
    let buyerReward = valueToEnter / BigInteger 10UL
    let referrerReward = valueToEnter * calculatedReferrerRewardPercAfter / BigInteger 100000

    let enteredEvent = receipt |> decodeFirstEvent<EnteredEventDTO>
    if referrer <> EthAddress.Zero then
        enteredEvent.BucketId |> should equal bucketToEnter
        enteredEvent.Buyer |> shouldEqualIgnoringCase buyer
        enteredEvent.BuyerReferralReward |> should equal buyerReward
        enteredEvent.Sender |> shouldEqualIgnoringCase sender
        enteredEvent.Referrer |> shouldEqualIgnoringCase referrer
        enteredEvent.ReferrerReferralReward |> should equal referrerReward
        enteredEvent.ValueEntered |> should equal valueToEnter
    else
        enteredEvent.BucketId |> should equal bucketToEnter
        enteredEvent.Buyer |> shouldEqualIgnoringCase buyer
        enteredEvent.BuyerReferralReward |> should equal BigInteger.Zero
        enteredEvent.Sender |> shouldEqualIgnoringCase sender
        enteredEvent.Referrer |> shouldEqualIgnoringCase referrer
        enteredEvent.ReferrerReferralReward |> should equal BigInteger.Zero
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
    let senderDaiBalanceAfter = DAI.Query "balanceOf" [| sender |]
    senderDaiBalanceAfter |> should equal (senderDaiBalanceBefore - valueToEnter)
    let bucketDaiBalanceAfter = DAI.Query "balanceOf" [| bucketSale.Address |]
    bucketDaiBalanceAfter |> should equal (bucketDaiBalanceBefore + valueToEnter)

    let buyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter; buyer |]
    buyForBucketAfter.ValueEntered |> should equal (buyForBucketBefore.ValueEntered + valueToEnter)
    buyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

    let bucketAfter = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter |]
    bucketAfter.TotalValueEntered |> should equal (bucketBefore.TotalValueEntered + valueToEnter)

    if referrer <> EthAddress.Zero then
        let buyerRewardBuyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; buyer |]
        buyerRewardBuyForBucketAfter.ValueEntered |> should equal (buyerRewardBuyForBucketBefore.ValueEntered + buyerReward)
        buyerRewardBuyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

        let referrerRewardBuyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; referrer |]
        referrerRewardBuyForBucketAfter.ValueEntered |> should equal (referrerRewardBuyForBucketBefore.ValueEntered + referrerReward)
        referrerRewardBuyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

        let referralBucketAfter = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter + BigInteger.One |]
        referralBucketAfter.TotalValueEntered |> should equal (referralBucketBefore.TotalValueEntered + referrerReward + buyerReward)
    else
        let buyerRewardBuyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; buyer |]
        buyerRewardBuyForBucketAfter.ValueEntered |> should equal BigInteger.Zero
        buyerRewardBuyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

        let referrerRewardBuyForBucketAfter = bucketSale.QueryObj<BuysOutputDTO> "buys" [| bucketToEnter + BigInteger.One; referrer |]
        buyerRewardBuyForBucketAfter.ValueEntered |> should equal BigInteger.Zero
        referrerRewardBuyForBucketAfter.BuyerTokensExited |> should equal BigInteger.Zero

        let referralBucketAfter = bucketSale.QueryObj<BucketsOutputDTO> "buckets" [| bucketToEnter + BigInteger.One |]
        referralBucketAfter.TotalValueEntered |> should equal (referralBucketBefore.TotalValueEntered)


[<Specification("BucketSale", "enter", 6)>]
[<Fact>]
let ``E006 - Can enter a bucket with no referrer``() =
    // arrange
    seedBucketWithFries()

    let currentBucket:BigInteger = bucketSale.Query "currentBucket" [||]

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
        fun bucketToEnter ->
            enterBucket
                sender
                buyer
                bucketToEnter
                valueToEnter
                referrer)

[<Specification("BucketSale", "enter", 8)>]
[<Fact>]
let ``E008 - Cannot enter a bucket beyond the designated bucket count - 1 (because of referrer)``() =
    let valueToEnter = BigInteger(10L)

    seedBucketWithFries()
    seedWithDAI forwarder.ContractPlug.Address valueToEnter

    let receiptDaiReceipt =  DAI.ExecuteFunctionFrom "approve" [| bucketSale.Address; valueToEnter |] forwarder
    receiptDaiReceipt |> shouldSucceed

    let bucketCount = bucketSale.Query "bucketCount" [||] // will be one bucket beyond what is allowed
    let receipt =
        bucketSale.ExecuteFunctionFrom
            "enter"
            [| ethConn.Account.Address; bucketCount - 1; 1UL; forwarder.ContractPlug.Address |]
            forwarder

    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage "invalid bucket id--past end of sale"


[<Specification("BucketSale", "enter", 9)>]
[<Fact>]
let ``E009 - Cannot enter a bucket if payment reverts (with referrer)``() =
    seedBucketWithFries()
    seedWithDAI forwarder.ContractPlug.Address (BigInteger(10UL))
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let receipt = bucketSale.ExecuteFunctionFrom "enter" [| ethConn.Account.Address; currentBucket; 1UL; forwarder.ContractPlug.Address |] forwarder
    let forwardEvent = forwarder.DecodeForwardedEvents receipt |> Seq.head
    forwardEvent |> shouldRevertWithMessage ""


[<Specification("BucketSale", "enter", 10)>]
[<Fact>]
let ``E010 - Can enter a bucket with no referrer``() =
    // arrange
    seedBucketWithFries()

    let currentBucket:BigInteger = bucketSale.Query "currentBucket" [||]

    let valueToEnter = BigInteger 10UL
    let buyer = ethConn.Account.Address
    let sender = ethConn.Account.Address

    let bucketsToEnter =
        rndRange (currentBucket |> int) (bucketCount - BigInteger 2UL  |> int)
        |> Seq.take 5
        |> Seq.toArray
        |> Array.append [| currentBucket; bucketCount - BigInteger 2UL |]

    Array.ForEach(
        bucketsToEnter,
        fun bucketToEnter ->
            enterBucket
                sender
                buyer
                bucketToEnter
                valueToEnter
                (makeAccount().Address))


[<Specification("BucketSale", "exit", 1)>]
[<Fact>]
let ``EX001 - Cannot exit a bucket that is not yet concluded``() =
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let firstReceipt = bucketSale.ExecuteFunctionFrom "exit" [| currentBucket; EthAddress.Zero |] forwarder

    let firstForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> firstReceipt
    firstForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    firstForwardEvent.Success |> should equal false
    firstForwardEvent.To |> should equal bucketSale.Address
    firstForwardEvent.Wei |> should equal BigInteger.Zero
    firstForwardEvent |> shouldRevertWithMessage "can only exit from concluded buckets"

    let laterBucker = rnd.Next((currentBucket + BigInteger.One) |> int32, (bucketCount - BigInteger 1UL) |> int32)
    let secondReceipt = bucketSale.ExecuteFunctionFrom "exit" [| laterBucker; EthAddress.Zero |] forwarder

    let secondForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> secondReceipt
    secondForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    secondForwardEvent.Success |> should equal false
    secondForwardEvent.To |> should equal bucketSale.Address
    secondForwardEvent.Wei |> should equal BigInteger.Zero
    secondForwardEvent |> shouldRevertWithMessage "can only exit from concluded buckets"

[<Specification("BucketSale", "exit", 2)>]
[<Fact>]
let ``EX002 - Cannot exit a bucket you did not enter``() =
    let currentBucket = bucketSale.Query "currentBucket" [||]
    let randomAddress = makeAccount().Address
    let firstReceipt = bucketSale.ExecuteFunctionFrom "exit" [| currentBucket - BigInteger.One; randomAddress |] forwarder

    let firstForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> firstReceipt
    firstForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    firstForwardEvent.Success |> should equal false
    firstForwardEvent.To |> should equal bucketSale.Address
    firstForwardEvent.Wei |> should equal BigInteger.Zero
    firstForwardEvent |> shouldRevertWithMessage "can't take out if you didn't put in"

[<Specification("BucketSale", "exit", 3)>]
[<Fact>]
let ``EX003 - Cannot exit a buy you have already exited``() =
    seedBucketWithFries()

    let currentBucket:BigInteger = bucketSale.Query "currentBucket" [||]
    let valueToEnter = BigInteger 10UL
    let buyer = ethConn.Account.Address
    let sender = ethConn.Account.Address

    enterBucket
        sender
        buyer
        currentBucket
        valueToEnter
        (makeAccount().Address)

    7UL * hours |> ethConn.TimeTravel 

    let firstReceipt = bucketSale.ExecuteFunctionFrom "exit" [| currentBucket; buyer |] forwarder
    let firstForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> firstReceipt
    firstForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    firstForwardEvent.Success |> should equal true
    firstForwardEvent.To |> should equal bucketSale.Address
    firstForwardEvent.Wei |> should equal BigInteger.Zero

    let secondReceipt = bucketSale.ExecuteFunctionFrom "exit" [| currentBucket; buyer |] forwarder
    let secondForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> secondReceipt
    secondForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    secondForwardEvent.Success |> should equal false
    secondForwardEvent.To |> should equal bucketSale.Address
    secondForwardEvent.Wei |> should equal BigInteger.Zero
    secondForwardEvent |> shouldRevertWithMessage "already withdrawn"


[<Specification("BucketSale", "exit", 3)>]
[<Fact>]
let ``EX004 - Cannot exit a bucket if the token transfer fails``() =
    seedBucketWithFries()

    let currentBucket:BigInteger = bucketSale.Query "currentBucket" [||]
    let valueToEnter = BigInteger 10UL
    let buyer = ethConn.Account.Address
    let sender = ethConn.Account.Address

    enterBucket
        sender
        buyer
        currentBucket
        valueToEnter
        (makeAccount().Address)

    7UL * hours |> ethConn.TimeTravel 
    
    let fryBalance = FRY.Query<BigInteger> "balanceOf" [| bucketSale.Address |] 
    
    let moveTokensData = FRY.FunctionData "transfer" [| makeAccount().Address; fryBalance |]
    let moveTokensForwardReciept = 
        bucketSale.ExecuteFunction 
            "forward" 
            [|
                FRY.Address
                moveTokensData.HexToByteArray()
                BigInteger.Zero 
            |]
    moveTokensForwardReciept |> shouldSucceed
    let forwardEvent = moveTokensForwardReciept.DecodeAllEvents<ForwardedEventDTO>() |> Seq.head
    let errorMessage = Encoding.ASCII.GetString(forwardEvent.Event.ResultData) 
    forwardEvent.Event.Success |> should equal true
    FRY.Query "balanceOf" [| bucketSale.Address |] |> should lessThan (bucketCount * bucketSupply)

    let exitReceipt = bucketSale.ExecuteFunctionFrom "exit" [| currentBucket; buyer |] forwarder
    let exitForwardEvent = decodeFirstEvent<DAIHard.Contracts.Forwarder.ContractDefinition.ForwardedEventDTO> exitReceipt
    exitForwardEvent.MsgSender |> shouldEqualIgnoringCase ethConn.Account.Address
    exitForwardEvent.Success |> should equal false
    exitForwardEvent.To |> should equal bucketSale.Address
    exitForwardEvent.Wei |> should equal BigInteger.Zero
    exitForwardEvent |> shouldRevertWithMessage "" //unknown internal revert of the ERC20, error is not necessarily known


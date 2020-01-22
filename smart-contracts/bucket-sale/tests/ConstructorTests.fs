module ConstructorTests

open FsUnit.Xunit
open Xunit
open Nethereum.RPC.Eth.DTOs
open Nethereum.Contracts
open TestBase
open System.Numerics
open System.Linq


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

[<Fact>]
let ``Cannot enter bucket sale with 0 amount``() =
    let currentBucket = bucketSale.QueryFunction "currentBucket" [||]
    let data = bucketSale.FunctionData "enter" [| ethConn.Account.Address; currentBucket; 0UL; zeroAddress |]
    let receipt = forwarder.SendTxAsync bucketSale.Address data (BigInteger(0)) |> runNow
    let event = forwarder.DecodeForwardedEvents receipt |> Seq.head
    event.Event.ResultAsRevertMessage |> should haveSubstring "can't buy nothing"

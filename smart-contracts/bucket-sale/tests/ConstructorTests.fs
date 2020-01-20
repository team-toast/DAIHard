module ConstructorTests

open FsUnit.Xunit
open Xunit
open Nethereum.RPC.Eth.DTOs
open Nethereum.Contracts
open TestBase

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

let query (contract: Contract) functionName paramArray =
    contract.GetFunction(functionName).CallAsync(paramArray) |> runNow

let shouldEqualIgnoringCase (a: string) (b: string) =
    let aString = a |> string
    let bString = b |> string
    should equal (aString.ToLower()) (bString.ToLower())

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

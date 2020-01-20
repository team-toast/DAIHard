module ConstructorTests

open Nethereum.Web3
open FsUnit.Xunit
open Xunit
open Microsoft.FSharp.Control
open FSharp.Data
open System
open Nethereum.RPC.Eth.DTOs
open System.Numerics
open Nethereum.Hex.HexTypes

type ABIType = JsonProvider<"../build/contracts/BucketSale.json">

let account = Accounts.Account("67b3534fb704a30cdb8c541b61becd5683662942a1b308e81822faf43c5d58ac")

let abi = ABIType.Load("../../../../build/contracts/BucketSale.json")

let minute = 60
let hour = minute * 60
let day = hour * 24
let startOfSale = DateTimeOffset(DateTime.Now.AddDays(-1.0)).ToUnixTimeSeconds
let bucketPeriod = 7 * hour
let bucketSupply = 50000
let zeroAddress = "0x0000000000000000000000000000000000000000"

let shouldSucceed (txr: TransactionReceipt) = txr.Status |> should equal "0x1"
let hexBigInteger (value: uint64) = HexBigInteger(BigInteger(value))
let maxGas = hexBigInteger 4000000UL

[<Fact>]
let ``Can construct the contract``() =
    let w3 = Web3(account, "http://localhost:8545")

    let accounts =
        w3.Eth.Accounts.SendRequestAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    accounts.Length |> should equal 10

    let constructorParams: obj list =
        [ account.Address; startOfSale; bucketPeriod; bucketSupply; zeroAddress; zeroAddress ]

    let deployTxReceipt =
        w3.Eth.DeployContract.SendRequestAndWaitForReceiptAsync
            (abi.Bytecode, account.Address, maxGas, constructorParams)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    deployTxReceipt |> shouldSucceed

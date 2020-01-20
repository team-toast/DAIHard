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
open System.Threading
open Newtonsoft
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Nethereum.Contracts

type ABIType = JsonProvider<"../build/contracts/BucketSale.json">

let minutes = 60UL
let hours = 60UL * minutes
let days = 24UL * hours
let startOfSale = DateTimeOffset(DateTime.Now.AddDays(-1.0)).ToUnixTimeSeconds() |> BigInteger
let bucketPeriod = 7UL * hours |> BigInteger
let bucketSupply = 50000UL |> BigInteger
let bucketCount = 100UL |> BigInteger
let zeroAddress = "0x0000000000000000000000000000000000000000"
let tokenOnSale = zeroAddress
let tokenSoldFor = zeroAddress
let bigInt (value: uint64) = BigInteger(value)
let hexBigInt (value: uint64) = HexBigInteger(bigInt value)

let runNow task =
    task
    |> Async.AwaitTask
    |> Async.RunSynchronously

type Abi(filename) =
    member _.JsonString = File.OpenText(filename).ReadToEnd()
    member this.AbiString = JsonConvert.DeserializeObject<JObject>(this.JsonString).GetValue("abi").ToString()
    member this.Bytecode = JsonConvert.DeserializeObject<JObject>(this.JsonString).GetValue("bytecode").ToString()

type EthereumConnection(nodeURI: string, privKey: string) =
    member _.Gas = hexBigInt 4000000UL
    member _.GasPrice = hexBigInt 1000000000UL
    member _.Account = Accounts.Account(privKey)
    member this.Web3 = Web3(this.Account, nodeURI)

    member this.DeployContractAsync (abi: Abi) (arguments: obj array) =
        this.Web3.Eth.DeployContract.SendRequestAndWaitForReceiptAsync
            (abi.AbiString, abi.Bytecode, this.Account.Address, this.Gas, this.GasPrice, hexBigInt 0UL, null, arguments)

    member this.SendTxAsync toAddress data (value: BigInteger) =
        let input: TransactionInput =
            TransactionInput(data, toAddress, this.Account.Address, this.Gas, this.GasPrice, HexBigInteger(value))

        this.Web3.Eth.TransactionManager.SendTransactionAndWaitForReceiptAsync(input, null)

type ContractPlug(ethConn: EthereumConnection, abi: Abi, address) =
    member _.Address = address
    member _.Contract = ethConn.Web3.Eth.GetContract(abi.AbiString, address)
    member this.Function functionName = this.Contract.GetFunction(functionName)
    member this.CallFunctionAsync functionName arguments = (this.Function functionName).CallAsync(arguments)
    member this.CallFunction functionName arguments = this.CallFunctionAsync functionName arguments |> runNow

let useRinkeby = false
let ganacheURI = "http://localhost:7545"
let rinkebyURI = "https://rinkeby.infura.io/v3/c48bc466281c4fefb3decad63c4fc815"
let ganachePrivKey = "67b3534fb704a30cdb8c541b61becd5683662942a1b308e81822faf43c5d58ac"
let rinkebyPrivKey = "5ca35a65adbd49af639a3686d7d438dba1bcef97cf1593cd5dd8fd79ca89fa3c"

let isRinkeby rinkeby notRinkeby =
    match useRinkeby with
    | true -> rinkeby
    | false -> notRinkeby

let ethConn =
    isRinkeby (EthereumConnection(rinkebyURI, rinkebyPrivKey)) (EthereumConnection(ganacheURI, ganachePrivKey))

let abi = ABIType.Load("../../../../build/contracts/BucketSale.json")
let shouldSucceed (txr: TransactionReceipt) = txr.Status |> should equal (hexBigInt 1UL)

let noParams: obj array = [||]

let getData abi functionName paramObj =
    ethConn.Web3.Eth.GetContract(abi, zeroAddress).GetFunction(functionName).GetData(paramObj)

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

let shouldEqualBigInteger a b =
    let aBig = a |> uint64
    let bBig = b |> uint64
    should equal aBig bBig

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

    contract.CallFunction "owner" [||] |> shouldEqualIgnoringCase ethConn.Account.Address
    contract.CallFunction "startOfSale" [||] |> should equal startOfSale
    contract.CallFunction "bucketPeriod" [||] |> should equal bucketPeriod
    contract.CallFunction "bucketSupply" [||] |> should equal bucketSupply
    contract.CallFunction "bucketCount" [||] |> should equal bucketCount
    contract.CallFunction "tokenOnSale" [||] |> shouldEqualIgnoringCase tokenOnSale
    contract.CallFunction "tokenSoldFor" [||] |> shouldEqualIgnoringCase tokenSoldFor

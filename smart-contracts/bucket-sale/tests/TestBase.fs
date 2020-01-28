module TestBase

open Nethereum.Web3
open FsUnit.Xunit
open Xunit
open Microsoft.FSharp.Control
open FSharp.Data
open System
open Nethereum.RPC.Eth.DTOs
open System.Numerics
open Nethereum.Hex.HexTypes
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Nethereum.Contracts
open Nethereum.ABI.FunctionEncoding.Attributes
open Nethereum.Hex.HexConvertors.Extensions
open System.Text
open Constants
open DTOs

type ABIType = JsonProvider<"../build/contracts/BucketSale.json">

let startOfSale = DateTimeOffset(DateTime.Now.AddDays(-1.0)).ToUnixTimeSeconds() |> BigInteger
let bucketPeriod = 7UL * hours |> BigInteger
let bucketSupply = 50000UL |> BigInteger
let bucketCount = 100UL |> BigInteger

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

    member this.SendTxAsync toAddress (value: BigInteger) data =
        let input: TransactionInput =
            TransactionInput(data, toAddress, this.Account.Address, this.Gas, this.GasPrice, HexBigInteger(value))

        this.Web3.Eth.TransactionManager.SendTransactionAndWaitForReceiptAsync(input, null)

type ContractPlug(ethConn: EthereumConnection, abi: Abi, address) =
    member _.Address = address
    member _.Contract = ethConn.Web3.Eth.GetContract(abi.AbiString, address)
    member this.Function functionName = this.Contract.GetFunction(functionName)
    member this.QueryFunctionAsync functionName arguments = (this.Function functionName).CallAsync(arguments)
    member this.QueryFunction functionName arguments = this.QueryFunctionAsync functionName arguments |> runNow
    member this.FunctionData functionName arguments = (this.Function functionName).GetData(arguments)
    member this.ExecuteFunctionAsync functionName arguments =
        this.FunctionData functionName arguments |> ethConn.SendTxAsync this.Address (BigInteger(0))
    member this.ExecuteFunction functionName arguments = this.ExecuteFunctionAsync functionName arguments |> runNow


type Forwarder(ethConn: EthereumConnection) =
    member _.EthConn = ethConn

    member _.ContractPlug =
        let abi = Abi("../../../../build/contracts/Forwarder.json")
        let deployTxReceipt = ethConn.DeployContractAsync abi [||] |> runNow
        ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

    member this.SendTxAsync (toAddress: string) (value: BigInteger) (data: string) =
        let data =
            this.ContractPlug.FunctionData "forward"
                [| toAddress
                   data.HexToByteArray() |]
        data |> ethConn.SendTxAsync this.ContractPlug.Address value

    member this.DecodeForwardedEvents(receipt: TransactionReceipt) =
        receipt.DecodeAllEvents<ForwardedEvent>() |> Seq.map (fun i -> i.Event)

        
[<System.AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
type SpecificationAttribute(contractName, functionName, specCode) =
    inherit Attribute()
    member _.ContractName: string = contractName
    member _.FunctionName: string = functionName
    member _.SpecCode: int = specCode

let useRinkeby = false
let ganacheURI = "http://localhost:7545"
let rinkebyURI = "https://rinkeby.infura.io/v3/c48bc466281c4fefb3decad63c4fc815"
let ganacheMnemonic = "join topple vapor pepper sell enter isolate pact syrup shoulder route token"
let ganachePrivKey = "689eb5e83bdc2ede1bb2d73b44c5315da21e2ce31e7507cd7fbb94caefd180b4"
let rinkebyPrivKey = "5ca35a65adbd49af639a3686d7d438dba1bcef97cf1593cd5dd8fd79ca89fa3c"

let isRinkeby rinkeby notRinkeby =
    match useRinkeby with
    | true -> rinkeby
    | false -> notRinkeby

let ethConn =
    isRinkeby (EthereumConnection(rinkebyURI, rinkebyPrivKey)) (EthereumConnection(ganacheURI, ganachePrivKey))

let forwarder = Forwarder(ethConn)

let shouldEqualIgnoringCase (a: string) (b: string) =
    let aString = a |> string
    let bString = b |> string
    should equal (aString.ToLower()) (bString.ToLower())

let shouldSucceed (txr: TransactionReceipt) = txr.Status |> should equal (hexBigInt 1UL)
let shouldFail (txr: TransactionReceipt) = txr.Status |> should equal (hexBigInt 0UL)

let shouldRevertWithMessage expectedMessage (forwardedEvent: ForwardedEvent) =
    match forwardedEvent.ResultAsRevertMessage with
    | None -> failwith "not a revert message"
    | Some actualMessage -> actualMessage |> should haveSubstring expectedMessage

let decodeEvents<'a when 'a: (new: unit -> 'a)> (receipt: TransactionReceipt) =
    receipt.DecodeAllEvents<'a>() |> Seq.map (fun e -> e.Event)

let decodeFirstEvent<'a when 'a: (new: unit -> 'a)> (receipt: TransactionReceipt) =
    decodeEvents<'a> receipt |> Seq.head

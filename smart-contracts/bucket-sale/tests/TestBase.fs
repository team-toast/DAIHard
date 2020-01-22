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
    member this.QueryFunctionAsync functionName arguments = (this.Function functionName).CallAsync(arguments)
    member this.QueryFunction functionName arguments = this.QueryFunctionAsync functionName arguments |> runNow
    member this.FunctionData functionName arguments = (this.Function functionName).GetData(arguments)

[<Event("Forwarded")>]
type ForwardedEvent() =

    [<Parameter("address", "_mgSender", 1, true)>]
    member val MsgSender: string = zeroAddress with get, set

    [<Parameter("address", "_to", 2, true)>]
    member val To: string = zeroAddress with get, set

    [<Parameter("bytes", "_data", 3, false)>]
    member val Data: byte array = [||] with get, set

    [<Parameter("uint256", "_wei", 4, false)>]
    member val Wei: BigInteger = BigInteger(0) with get, set

    [<Parameter("bool", "_success", 5, false)>]
    member val Success: bool = false with get, set

    [<Parameter("bytes", "_resultData", 6, false)>]
    member val ResultData: byte array = [||] with get, set

    member this.ResultAsRevertMessage = Encoding.ASCII.GetString(this.ResultData)

type Forwarder(ethConn: EthereumConnection) =
    member _.EthConn = ethConn

    member _.ContractPlug =
        let abi = Abi("../../../../build/contracts/Forwarder.json")
        let deployTxReceipt = ethConn.DeployContractAsync abi [||] |> runNow
        ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

    member this.SendTxAsync (toAddress: string) (data: string) (value: BigInteger) =
        let data =
            this.ContractPlug.FunctionData "forward"
                [| toAddress
                   data.HexToByteArray() |]
        ethConn.SendTxAsync this.ContractPlug.Address data value

    member this.DecodeForwardedEvents(receipt: TransactionReceipt) = receipt.DecodeAllEvents<ForwardedEvent>()

type SpecificationAttribute(contractName, functionName, specCode) =
    inherit Attribute()
    member _.ContractName: string = contractName
    member _.FunctionName: string = functionName
    member _.SpecCode: int = specCode

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

let forwarder = Forwarder(ethConn)

let shouldEqualIgnoringCase (a: string) (b: string) =
    let aString = a |> string
    let bString = b |> string
    should equal (aString.ToLower()) (bString.ToLower())

let shouldSucceed (txr: TransactionReceipt) = txr.Status |> should equal (hexBigInt 1UL)
let shouldFail (txr: TransactionReceipt) = txr.Status |> should equal (hexBigInt 0UL)

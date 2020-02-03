module TestBase

open Nethereum.Web3
open FsUnit.Xunit
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
open Nethereum.Hex.HexConvertors.Extensions
open System.Text
open Constants
open DAIHard.Contracts.Forwarder.ContractDefinition
open System.Threading.Tasks
open Nethereum.Web3.Accounts

type ABIType = JsonProvider<"../build/contracts/BucketSale.json">

let rnd = Random()

let rec rndRange min max  = 
    seq { 
        yield rnd.Next(min,max) |> BigInteger
        yield! rndRange min max
        }

let startOfSale = DateTimeOffset(DateTime.Now.AddDays(-1.0)).ToUnixTimeSeconds() |> BigInteger
let bucketPeriod = 7UL * hours |> BigInteger
let bucketSupply = 50000UL |> BigInteger
let bucketCount = 100UL |> BigInteger

let tokenOnSale = zeroAddress
let tokenSoldFor = zeroAddress
let bigInt (value: uint64) = BigInteger(value)
let hexBigInt (value: uint64) = HexBigInteger(bigInt value)

let runNow (task:Task<'T>) =
    task
    |> Async.AwaitTask
    |> Async.RunSynchronously

type Abi(filename) =
    member val JsonString = File.OpenText(filename).ReadToEnd()
    member this.AbiString = JsonConvert.DeserializeObject<JObject>(this.JsonString).GetValue("abi").ToString()
    member this.Bytecode = JsonConvert.DeserializeObject<JObject>(this.JsonString).GetValue("bytecode").ToString()

type IAsyncTxSender =
    abstract member SendTxAsync : string -> BigInteger -> string -> Task<TransactionReceipt> 

type EthereumConnection(nodeURI: string, privKey: string) =
    interface IAsyncTxSender with
        member this.SendTxAsync toAddress value data = 
            let input: TransactionInput =
                TransactionInput(
                    data, 
                    toAddress, 
                    this.Account.Address, 
                    this.Gas, this.GasPrice, 
                    HexBigInteger(value))
            this.Web3.Eth.TransactionManager.SendTransactionAndWaitForReceiptAsync(input, null)

    member val public Gas = hexBigInt 4000000UL
    member val public GasPrice = hexBigInt 1000000000UL
    member val public Account = Accounts.Account(privKey)
    member val public Web3 = Web3(Accounts.Account(privKey), nodeURI)

    member this.DeployContractAsync (abi: Abi) (arguments: obj array) =
        this.Web3.Eth.DeployContract.SendRequestAndWaitForReceiptAsync(
            abi.AbiString, 
            abi.Bytecode, 
            this.Account.Address, 
            this.Gas, this.GasPrice, 
            hexBigInt 0UL, 
            null, 
            arguments)
    
    member this.TimeTravel seconds = 
        this.Web3.Client.SendRequestAsync(method = "evm_increaseTime", paramList = [| seconds |]) 
        |> Async.AwaitTask 
        |> Async.RunSynchronously
        this.Web3.Client.SendRequestAsync(method = "evm_mine", paramList = [||]) 
        |> Async.AwaitTask 
        |> Async.RunSynchronously


type Profile = { FunctionName: string; Duration: string }

let profileMe f =
    let start = DateTime.Now
    let result = f()
    let duration = DateTime.Now - start
    (f.GetType(), duration) |> printf "(Functio, Duration) = %A\n"
    result


type ContractPlug(ethConn: EthereumConnection, abi: Abi, address) =
    member val public Address = address

    member val public Contract = 
        ethConn.Web3.Eth.GetContract(abi.AbiString, address)
        
    member this.Function functionName = 
        this.Contract.GetFunction(functionName)

    member this.QueryObjAsync<'a when 'a: (new: unit -> 'a)> functionName arguments = 
        (this.Function functionName).CallDeserializingToObjectAsync<'a> (arguments)

    member this.QueryObj<'a when 'a: (new: unit -> 'a)> functionName arguments = 
        this.QueryObjAsync<'a> functionName arguments |> runNow

    member this.QueryAsync<'a> functionName arguments = 
        (this.Function functionName).CallAsync<'a> (arguments)

    member this.Query<'a> functionName arguments = 
        this.QueryAsync<'a> functionName arguments |> runNow

    member this.FunctionData functionName arguments = 
        (this.Function functionName).GetData(arguments)

    member this.ExecuteFunctionFromAsync functionName arguments (connection:IAsyncTxSender) = 
        this.FunctionData functionName arguments |> connection.SendTxAsync this.Address (BigInteger(0))

    member this.ExecuteFunctionFrom functionName arguments connection = 
        this.ExecuteFunctionFromAsync functionName arguments connection |> runNow

    member this.ExecuteFunctionAsync functionName arguments = 
        this.ExecuteFunctionFromAsync functionName arguments ethConn

    member this.ExecuteFunction functionName arguments = 
        this.ExecuteFunctionAsync functionName arguments |> runNow
            

type Forwarder(ethConn: EthereumConnection) =
    member val public EthConn = ethConn
    member val public AsyncTxSender = ethConn :> IAsyncTxSender

    member val public  ContractPlug =
        let abi = Abi("../../../../build/contracts/Forwarder.json")
        let deployTxReceipt = ethConn.DeployContractAsync abi [||] |> runNow
        ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

    interface IAsyncTxSender with
        member this.SendTxAsync(toAddress: string) (value: BigInteger) (data: string): Threading.Tasks.Task<TransactionReceipt> = 
            let data =
                this.ContractPlug.FunctionData "forward"
                    [| toAddress
                       data.HexToByteArray() |]
            data |> this.AsyncTxSender.SendTxAsync this.ContractPlug.Address value

    member this.DecodeForwardedEvents(receipt: TransactionReceipt) =
        receipt.DecodeAllEvents<ForwardedEventDTO>() |> Seq.map (fun i -> i.Event)

type ForwardedEventDTO with
    member this.ResultAsRevertMessage =
        match this.Success with
        | true -> None
        | _ -> Some(Encoding.ASCII.GetString(this.ResultData))

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

let shouldRevertWithMessage expectedMessage (forwardedEvent: ForwardedEventDTO) =
    match forwardedEvent.ResultAsRevertMessage with
    | None -> failwith "not a revert message"
    | Some actualMessage -> actualMessage |> should haveSubstring expectedMessage

let decodeEvents<'a when 'a: (new: unit -> 'a)> (receipt: TransactionReceipt) =
    receipt.DecodeAllEvents<'a>() |> Seq.map (fun e -> e.Event)

let decodeFirstEvent<'a when 'a: (new: unit -> 'a)> (receipt: TransactionReceipt) =
    decodeEvents<'a> receipt |> Seq.head

let makeAccount() =
    let ecKey = Nethereum.Signer.EthECKey.GenerateKey();
    let privateKey = ecKey.GetPrivateKeyAsBytes().ToHex();
    Account(privateKey);
module DTOs

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

// struct Bucket
// {
//     uint totalValueEntered;
// }
type Bucket() =
    [<Parameter("uint256", "_totalValueEntered", 1, false)>]
    member val TotalValueEntered: BigInteger = BigInteger(0) with get, set

// event Entered(
//     address _msgSender,
//     uint256 _bucketId,
//     address indexed _buyer,
//     uint _valueEntered,
//     uint _buyerReferralReward,
//     address indexed _referrer,
//     uint _referrerReferralReward);
[<Event("Entered")>]
type EnteredEvent() =

    [<Parameter("address", "_msgSender", 1, true)>]
    member val MsgSender: string = zeroAddress with get, set

    [<Parameter("uint256", "_bucketId", 2, false)>]
    member val BucketId: BigInteger = BigInteger(0) with get, set

    [<Parameter("address", "_buyer", 3, true)>]
    member val Buyer: string = zeroAddress with get, set

    [<Parameter("uint256", "_valueEntered", 4, false)>]
    member val ValueEntered: BigInteger = BigInteger(0) with get, set

    [<Parameter("uint256", "_buyerReferralReward", 5, false)>]
    member val BuyerReferralReward: BigInteger = BigInteger(0) with get, set

    [<Parameter("address", "_referrer", 6, true)>]
    member val Referrer: string = zeroAddress with get, set

    [<Parameter("uint256", "_referrerReferralReward", 7, false)>]
    member val ReferrerReferralReward: BigInteger = BigInteger(0) with get, set


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

    member this.ResultAsRevertMessage =
        match this.Success with
        | true -> None
        | _ -> Some(Encoding.ASCII.GetString(this.ResultData))

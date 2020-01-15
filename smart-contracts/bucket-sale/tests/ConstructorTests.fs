module ConstructorTests

open Nethereum.Web3
open FsUnit.Xunit
open Xunit
open Microsoft.FSharp.Control
open FSharp.Data

type ABIType = JsonProvider<"../build/contracts/BucketSale.json">

let account = Accounts.Account("67b3534fb704a30cdb8c541b61becd5683662942a1b308e81822faf43c5d58ac")

[<Fact>]
let ``Check we can connect``() =
    let w3 = Web3(account, "http://localhost:8545")

    let accounts =
        w3.Eth.Accounts.SendRequestAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously
    accounts.Length |> should equal 10

// let deployTx =
//     w3.Eth.DeployContract.SendRequestAndWaitForReceiptAsync(

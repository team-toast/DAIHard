module BucketSaleTestBase

open TestBase
open System.Numerics
open FsUnit.Xunit

let DAI =
    let abi = Abi("../../../../build/contracts/TestToken.json")

    let deployTxReceipt =
        ethConn.DeployContractAsync abi
            [| "MCD DAI stable coin"
               "DAI"
               ethConn.Account.Address
               bucketSupply * bucketCount * BigInteger(100UL) |]
        |> runNow

    let result = ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)
    result.Query "balanceOf" [| ethConn.Account.Address |] |> should equal (bucketSupply * bucketCount * BigInteger(100UL) * BigInteger(1000000000000000000UL))
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
            [| ethConn.Account.Address; startOfSale; bucketPeriod; bucketSupply; bucketCount; FRY.Address; DAI.Address |]
        |> runNow

    ContractPlug(ethConn, abi, deployTxReceipt.ContractAddress)

let seedBucketWithFries() =
    let frySupplyBefore = FRY.Query "balanceOf" [| bucketSale.Address |]
    let transferFryTxReceipt =
        FRY.ExecuteFunction "transfer"
            [| bucketSale.Address
               bucketSupply * bucketCount |]
    transferFryTxReceipt |> shouldSucceed
    FRY.Query "balanceOf" [| bucketSale.Address |] |> should equal (frySupplyBefore + bucketSupply * bucketCount)

let seedWithDAI (recipient:string) (amount:BigInteger) =
    let balanceBefore = DAI.Query "balanceOf" [| recipient |] 
    let transferDaiTxReceipt = DAI.ExecuteFunction "transfer" [| recipient; amount |]
    transferDaiTxReceipt |> shouldSucceed
    DAI.Query "balanceOf" [| recipient |] |> should equal (balanceBefore + amount)
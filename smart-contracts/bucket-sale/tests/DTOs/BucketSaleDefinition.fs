namespace DAIHard.Contracts.BucketSale.ContractDefinition

open System.Numerics
open Nethereum.ABI.FunctionEncoding.Attributes
open Nethereum.Contracts

type BucketSaleDeployment(byteCode: string) =
    inherit ContractDeploymentMessage(byteCode)

    static let BYTECODE =
        "0x608060405234801561001057600080fd5b50604051611cc1380380611cc1833981810160405260e081101561003357600080fd5b810190808051906020019092919080519060200190929190805190602001909291908051906020019092919080519060200190929190805190602001909291908051906020019092919050505086600360006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055508560048190555084600581905550836006819055508260078190555081600960006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555080600a60006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555050505050505050611b4c806101756000396000f3fe608060405234801561001057600080fd5b506004361061013e576000357c01000000000000000000000000000000000000000000000000000000009004806397b2fd45116100ca578063c4aaeb1a1161008e578063c4aaeb1a14610447578063cbc4319914610465578063cff40759146104dd578063dc5fe0251461052b578063f838b825146106945761013e565b806397b2fd45146103535780639b51fb0d146103ab578063a03effd1146103ed578063b80777ea1461040b578063c2f5673e146104295761013e565b80634f127aae116101115780634f127aae1461022b57806360e6a4401461024957806389ce96c6146102935780638da5cb5b146102b15780639361265f146102fb5761013e565b80633261933d1461014357806342f2b6ec1461016157806347e3baaa146101c35780634b42442e1461020d575b600080fd5b61014b6106fd565b6040518082815260200191505060405180910390f35b6101ad6004803603604081101561017757600080fd5b8101908080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919050505061071a565b6040518082815260200191505060405180910390f35b6101cb6107be565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b6102156107e4565b6040518082815260200191505060405180910390f35b6102336107ea565b6040518082815260200191505060405180910390f35b610251610821565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b61029b610847565b6040518082815260200191505060405180910390f35b6102b961084d565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b61033d6004803603602081101561031157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610873565b6040518082815260200191505060405180910390f35b6103956004803603602081101561036957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610959565b6040518082815260200191505060405180910390f35b6103d7600480360360208110156103c157600080fd5b8101908080359060200190929190505050610971565b6040518082815260200191505060405180910390f35b6103f561098f565b6040518082815260200191505060405180910390f35b610413610995565b6040518082815260200191505060405180910390f35b61043161099d565b6040518082815260200191505060405180910390f35b61044f6109a3565b6040518082815260200191505060405180910390f35b6104db6004803603608081101561047b57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506109a9565b005b610529600480360360408110156104f357600080fd5b8101908080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610e04565b005b61060e6004803603606081101561054157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019064010000000081111561057e57600080fd5b82018360208201111561059057600080fd5b803590602001918460018302840111640100000000831117156105b257600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f8201169050808301925050505050505091929192908035906020019092919050505061119a565b604051808315151515815260200180602001828103825283818151815260200191508051906020019080838360005b8381101561065857808201518184015260208101905061063d565b50505050905090810190601f1680156106855780820380516001836020036101000a031916815260200191505b50935050505060405180910390f35b6106e0600480360360408110156106aa57600080fd5b8101908080359060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919050505061147e565b604051808381526020018281526020019250505060405180910390f35b6000610715600a6103e86114af90919063ffffffff16565b905090565b600080600080858152602001908152602001600020905060006001600086815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002090506107b482600001546107a683600001546006546114af90919063ffffffff16565b61155290919063ffffffff16565b9250505092915050565b600a60009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60085481565b600061081c60055461080e600454610800610995565b6115e190919063ffffffff16565b61155290919063ffffffff16565b905090565b600960009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60065481565b600360009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1681565b60008073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff1614156108b25760009050610954565b600061090e670de0b6b3a7640000600260008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205461155290919063ffffffff16565b9050600061093a61092b600a6103e86114af90919063ffffffff16565b8361166a90919063ffffffff16565b9050600061094b620186a0836116f2565b90508093505050505b919050565b60026020528060005260406000206000915090505481565b60006020528060005260406000206000915090508060000154905081565b60045481565b600042905090565b60075481565b60055481565b6109b483858461170b565b610a0682600260008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205461166a90919063ffffffff16565b600260008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506000600a60009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166323b872dd3330866040518463ffffffff167c0100000000000000000000000000000000000000000000000000000000028152600401808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018281526020019350505050602060405180830381600087803b158015610b4457600080fd5b505af1158015610b58573d6000803e3d6000fd5b505050506040513d6020811015610b6e57600080fd5b8101908080519060200190929190505050905080610bf4576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260158152602001807f656e746572207472616e73666572206661696c6564000000000000000000000081525060200191505060405180910390fd5b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff1614610d66576000610c59620186a0610c4b610c3c6106fd565b876114af90919063ffffffff16565b61155290919063ffffffff16565b90506000610c8d620186a0610c7f610c7087610873565b886114af90919063ffffffff16565b61155290919063ffffffff16565b9050610cad610ca660018861166a90919063ffffffff16565b888461170b565b610ccb610cc460018861166a90919063ffffffff16565b858361170b565b8373ffffffffffffffffffffffffffffffffffffffff168773ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167f161e0456cd3270520befa83f5fdd74084ad38cd70a096dcf24ccf7edc368b04f898987876040518085815260200184815260200183815260200182815260200194505050505060405180910390a45050610dfd565b600073ffffffffffffffffffffffffffffffffffffffff168573ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167f161e0456cd3270520befa83f5fdd74084ad38cd70a096dcf24ccf7edc368b04f87876000806040518085815260200184815260200183815260200182815260200194505050505060405180910390a45b5050505050565b610e0c6107ea565b8210610e63576040517f08c379a0000000000000000000000000000000000000000000000000000000008152600401808060200182810382526024815260200180611ad16024913960400191505060405180910390fd5b60006001600084815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002090506000816000015411610f14576040517f08c379a0000000000000000000000000000000000000000000000000000000008152600401808060200182810382526023815260200180611af56023913960400191505060405180910390fd5b6000816001015414610f8e576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260118152602001807f616c72656164792077697468647261776e00000000000000000000000000000081525060200191505060405180910390fd5b610f98838361071a565b8160010181905550610fb9816001015460085461166a90919063ffffffff16565b6008819055506000600960009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663a9059cbb8484600101546040518363ffffffff167c0100000000000000000000000000000000000000000000000000000000028152600401808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200182815260200192505050602060405180830381600087803b15801561108a57600080fd5b505af115801561109e573d6000803e3d6000fd5b505050506040513d60208110156110b457600080fd5b810190808051906020019092919050505090508061113a576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260148152602001807f65786974207472616e73666572206661696c656400000000000000000000000081525060200191505060405180910390fd5b8273ffffffffffffffffffffffffffffffffffffffff167f0808b45a422e0acd47a625c74fff3eb8d6d4dd063e0845deb1e57581c27b32f5858460010154604051808381526020018281526020019250505060405180910390a250505050565b60006060600360009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff1614611261576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252600a8152602001807f6f6e6c79206f776e65720000000000000000000000000000000000000000000081525060200191505060405180910390fd5b600060608673ffffffffffffffffffffffffffffffffffffffff1685876040518082805190602001908083835b602083106112b1578051825260208201915060208101905060208303925061128e565b6001836020036101000a03801982511681845116808217855250505050505090500191505060006040518083038185875af1925050503d8060008114611313576040519150601f19603f3d011682016040523d82523d6000602084013e611318565b606091505b50915091507f7b655daaeef97843e8691c590dde6d4925d0015d198c34570bb7b6f3e967f0558787878585604051808673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001806020018581526020018415151515815260200180602001838103835287818151815260200191508051906020019080838360005b838110156113c95780820151818401526020810190506113ae565b50505050905090810190601f1680156113f65780820380516001836020036101000a031916815260200191505b50838103825284818151815260200191508051906020019080838360005b8381101561142f578082015181840152602081019050611414565b50505050905090810190601f16801561145c5780820380516001836020036101000a031916815260200191505b5097505050505050505060405180910390a18181935093505050935093915050565b6001602052816000526040600020602052806000526040600020600091509150508060000154908060010154905082565b6000808314156114c2576000905061154c565b60008284029050828482816114d357fe5b0414611547576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260108152602001807f75696e74323536206f766572666c6f770000000000000000000000000000000081525060200191505060405180910390fd5b809150505b92915050565b60008082116115c9576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260118152602001807f43616e277420646976696465206279203000000000000000000000000000000081525060200191505060405180910390fd5b60008284816115d457fe5b0490508091505092915050565b600082821115611659576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260118152602001807f75696e7432353620756e646572666c6f7700000000000000000000000000000081525060200191505060405180910390fd5b600082840390508091505092915050565b6000808284019050838110156116e8576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260108152602001807f75696e74323536206f766572666c6f770000000000000000000000000000000081525060200191505060405180910390fd5b8091505092915050565b60008183106117015781611703565b825b905092915050565b6117136107ea565b831015611788576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260198152602001807f63616e6e6f7420656e7465722070617374206275636b6574730000000000000081525060200191505060405180910390fd5b60075483106117e2576040517f08c379a0000000000000000000000000000000000000000000000000000000008152600401808060200182810382526023815260200180611aae6023913960400191505060405180910390fd5b60008111611858576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260118152602001807f63616e277420627579206e6f7468696e6700000000000000000000000000000081525060200191505060405180910390fd5b61188060065461187260018661166a90919063ffffffff16565b6114af90919063ffffffff16565b61198a600854600960009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166370a08231306040518263ffffffff167c0100000000000000000000000000000000000000000000000000000000028152600401808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060206040518083038186803b15801561194157600080fd5b505afa158015611955573d6000803e3d6000fd5b505050506040513d602081101561196b57600080fd5b810190808051906020019092919050505061166a90919063ffffffff16565b10156119fe576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252601b8152602001807f696e73756666696369656e7420746f6b656e7320746f2073656c6c000000000081525060200191505060405180910390fd5b60006001600085815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000209050611a6982826000015461166a90919063ffffffff16565b816000018190555060008060008681526020019081526020016000209050611a9e83826000015461166a90919063ffffffff16565b8160000181905550505050505056fe696e76616c6964206275636b65742069642d2d7061737420656e64206f662073616c6563616e206f6e6c7920657869742066726f6d20636f6e636c75646564206275636b65747363616e27742074616b65206f757420696620796f75206469646e27742070757420696ea265627a7a723158209b554f6cfe70ae7a02cfd054edd5c26dfd93a5a23f44f516ec4a9b6aa453b63d64736f6c63430005100032"

    new() = BucketSaleDeployment(BYTECODE)

    [<Parameter("address", "_owner", 1)>]
    member val public Owner = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_startOfSale", 2)>]
    member val public StartOfSale = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "_bucketPeriod", 3)>]
    member val public BucketPeriod = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "_bucketSupply", 4)>]
    member val public BucketSupply = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "_bucketCount", 5)>]
    member val public BucketCount = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_tokenOnSale", 6)>]
    member val public TokenOnSale = Unchecked.defaultof<string> with get, set

    [<Parameter("address", "_tokenSoldFor", 7)>]
    member val public TokenSoldFor = Unchecked.defaultof<string> with get, set


[<Function("bucketCount", "uint256")>]
type BucketCountFunction() =
    inherit FunctionMessage()




[<Function("bucketPeriod", "uint256")>]
type BucketPeriodFunction() =
    inherit FunctionMessage()




[<Function("bucketSupply", "uint256")>]
type BucketSupplyFunction() =
    inherit FunctionMessage()




[<Function("buckets", "uint256")>]
type BucketsFunction() =
    inherit FunctionMessage()

    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set

[<FunctionOutput>]
type ForwardOutputDTO() =
    inherit FunctionOuputDTO()

    [<Parameter("bool", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<bool> with get, set

    [<Parameter("bytes", "", 2)>]
    member val public ReturnValue2 = Unchecked.defaultof<byte []> with get, set


[<FunctionOutput>]
type BuysOutputDTO() =
    inherit FunctionOuputDTO() 

    [<Parameter("uint256", "valueEntered", 1)>]
    member val public ValueEntered = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "buyerTokensExited", 2)>]
    member val public BuyerTokensExited = Unchecked.defaultof<BigInteger> with get, set


[<Function("buys", typeof<BuysOutputDTO>)>]
type BuysFunction() =
    inherit FunctionMessage()

    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "", 2)>]
    member val public ReturnValue2 = Unchecked.defaultof<string> with get, set


[<Function("owner", "address")>]
type OwnerFunction() =
    inherit FunctionMessage()




[<Function("referredTotal", "uint256")>]
type ReferredTotalFunction() =
    inherit FunctionMessage()

    [<Parameter("address", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<string> with get, set


[<Function("startOfSale", "uint256")>]
type StartOfSaleFunction() =
    inherit FunctionMessage()




[<Function("tokenOnSale", "address")>]
type TokenOnSaleFunction() =
    inherit FunctionMessage()




[<Function("tokenSoldFor", "address")>]
type TokenSoldForFunction() =
    inherit FunctionMessage()




[<Function("totalExitedTokens", "uint256")>]
type TotalExitedTokensFunction() =
    inherit FunctionMessage()




[<Function("timestamp", "uint256")>]
type TimestampFunction() =
    inherit FunctionMessage()




[<Function("forward", typeof<ForwardOutputDTO>)>]
type ForwardFunction() =
    inherit FunctionMessage()

    [<Parameter("address", "_to", 1)>]
    member val public To = Unchecked.defaultof<string> with get, set

    [<Parameter("bytes", "_data", 2)>]
    member val public Data = Unchecked.defaultof<byte []> with get, set

    [<Parameter("uint256", "_wei", 3)>]
    member val public Wei = Unchecked.defaultof<BigInteger> with get, set


[<Function("currentBucket", "uint256")>]
type CurrentBucketFunction() =
    inherit FunctionMessage()




[<Function("enter")>]
type EnterFunction() =
    inherit FunctionMessage()

    [<Parameter("address", "_buyer", 1)>]
    member val public Buyer = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_bucketId", 2)>]
    member val public BucketId = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "_amount", 3)>]
    member val public Amount = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_referrer", 4)>]
    member val public Referrer = Unchecked.defaultof<string> with get, set


[<Function("exit")>]
type ExitFunction() =
    inherit FunctionMessage()

    [<Parameter("uint256", "_bucketId", 1)>]
    member val public BucketId = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_buyer", 2)>]
    member val public Buyer = Unchecked.defaultof<string> with get, set


[<Function("buyerReferralRewardPerc", "uint256")>]
type BuyerReferralRewardPercFunction() =
    inherit FunctionMessage()




[<Function("referrerReferralRewardPerc", "uint256")>]
type ReferrerReferralRewardPercFunction() =
    inherit FunctionMessage()

    [<Parameter("address", "_referrerAddress", 1)>]
    member val public ReferrerAddress = Unchecked.defaultof<string> with get, set


[<Function("calculateExitableTokens", "uint256")>]
type CalculateExitableTokensFunction() =
    inherit FunctionMessage()

    [<Parameter("uint256", "_bucketId", 1)>]
    member val public BucketId = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_buyer", 2)>]
    member val public Buyer = Unchecked.defaultof<string> with get, set


[<Event("Entered")>]
type EnteredEventDTO() =
    inherit EventDTO()

    [<Parameter("address", "_sender", 1, true)>]
    member val Sender = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_bucketId", 2, false)>]
    member val BucketId = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_buyer", 3, true)>]
    member val Buyer = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_valueEntered", 4, false)>]
    member val ValueEntered = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("uint256", "_buyerReferralReward", 5, false)>]
    member val BuyerReferralReward = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_referrer", 6, true)>]
    member val Referrer = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_referrerReferralReward", 7, false)>]
    member val ReferrerReferralReward = Unchecked.defaultof<BigInteger> with get, set


[<Event("Exited")>]
type ExitedEventDTO() =
    inherit EventDTO()

    [<Parameter("uint256", "_bucketId", 1, false)>]
    member val BucketId = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("address", "_buyer", 2, true)>]
    member val Buyer = Unchecked.defaultof<string> with get, set

    [<Parameter("uint256", "_tokensExited", 3, false)>]
    member val TokensExited = Unchecked.defaultof<BigInteger> with get, set


[<Event("Forwarded")>]
type ForwardedEventDTO() =
    inherit EventDTO()

    [<Parameter("address", "_to", 1, false)>]
    member val To = Unchecked.defaultof<string> with get, set

    [<Parameter("bytes", "_data", 2, false)>]
    member val Data = Unchecked.defaultof<byte []> with get, set

    [<Parameter("uint256", "_wei", 3, false)>]
    member val Wei = Unchecked.defaultof<BigInteger> with get, set

    [<Parameter("bool", "_success", 4, false)>]
    member val Success = Unchecked.defaultof<bool> with get, set

    [<Parameter("bytes", "_resultData", 5, false)>]
    member val ResultData = Unchecked.defaultof<byte []> with get, set


[<FunctionOutput>]
type BucketCountOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type BucketPeriodOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type BucketSupplyOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type BucketsOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "totalValueEntered", 1)>]
    member val public TotalValueEntered = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type OwnerOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("address", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<string> with get, set


[<FunctionOutput>]
type ReferredTotalOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type StartOfSaleOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type TokenOnSaleOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("address", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<string> with get, set


[<FunctionOutput>]
type TokenSoldForOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("address", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<string> with get, set


[<FunctionOutput>]
type TotalExitedTokensOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type TimestampOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "_now", 1)>]
    member val public Now = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type CurrentBucketOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type BuyerReferralRewardPercOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type ReferrerReferralRewardPercOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set


[<FunctionOutput>]
type CalculateExitableTokensOutputDTO() =
    inherit FunctionOuputDTO()
    [<Parameter("uint256", "", 1)>]
    member val public ReturnValue1 = Unchecked.defaultof<BigInteger> with get, set

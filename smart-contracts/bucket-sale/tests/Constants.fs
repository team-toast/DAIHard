module Constants

type EthAddress(rawString: string) =
    static member Zero = "0x0000000000000000000000000000000000000000"
    member _.StringValue = rawString.ToLower()

let minutes = 60UL
let hours = 60UL * minutes
let days = 24UL * hours
let zeroAddress = EthAddress.Zero
module Contracts.ToastytradeSellExtras exposing (..)

-- Library

import BigInt exposing (BigInt)

type Phase
  = Open
  | Committed
  | Claimed
  | Closed

bigIntToPhase : BigInt -> Result String Phase
bigIntToPhase phase =
  let
    phaseInt = Result.withDefault 99 ( BigInt.toString phase |> String.toInt )
  in
    case phaseInt of
      0 -> Ok Open
      1 -> Ok Committed
      2 -> Ok Claimed
      3 -> Ok Closed
      _ -> Err "Invalid Toastytrade.Phase enum! How did this happen? Did you forget to update the ABI with a new phase?"

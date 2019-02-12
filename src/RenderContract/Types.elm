module RenderContract.Types exposing (Msg(..), ViewContext, ViewMode(..))

import Contracts.Types
import Time


type ViewMode
    = Draft
    | Active ViewContext


type alias ViewContext =
    { state : Contracts.Types.State
    , currentTime : Time.Posix
    , userIsInitiator : Bool
    , userIsResponder : Bool
    }


type Msg
    = Poke
    | Commit
    | Recall
    | Claim
    | Release
    | Burn

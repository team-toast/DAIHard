module RenderContract.Types exposing (Msg(..), ViewContext, ViewMode(..), generateContext)

import Contracts.Types
import Eth.Types exposing (Address)
import Time


type ViewMode
    = Draft
    | Active ViewContext


type alias ViewContext =
    { state : Contracts.Types.State
    , currentTime : Time.Posix
    , userIsInitiator : Bool
    , userIsResponder : Bool
    , userIsSeller : Bool
    , userIsBuyer : Bool
    }


type Msg
    = Poke
    | Commit
    | Recall
    | Claim
    | Abort
    | Release
    | Burn


generateContext : Contracts.Types.CreateParameters -> Contracts.Types.State -> Address -> Time.Posix -> ViewContext
generateContext parameters state userAddress time =
    let
        userIsInitiator =
            userAddress == parameters.initiatorAddress

        userIsResponder =
            case state.responder of
                Just responder ->
                    userAddress == responder

                Nothing ->
                    False

        ( userIsBuyer, userIsSeller ) =
            case parameters.openMode of
                Contracts.Types.BuyerOpened ->
                    ( userIsInitiator, userIsResponder )

                Contracts.Types.SellerOpened ->
                    ( userIsResponder, userIsInitiator )
    in
    { state = state
    , currentTime = time
    , userIsInitiator = userIsInitiator
    , userIsResponder = userIsResponder
    , userIsBuyer = userIsBuyer
    , userIsSeller = userIsSeller
    }

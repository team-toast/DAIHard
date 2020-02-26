module ChainCmd exposing (ChainCmd(..), custom, execute, map, none)

import Eth.Sentry.Tx as TxSentry
import Eth.Types
import UserNotice as UN


type ChainCmd msg
    = CustomSend (TxSentry.CustomSend msg) Eth.Types.Send
    | None


none : ChainCmd msg
none =
    None


custom : TxSentry.CustomSend msg -> Eth.Types.Send -> ChainCmd msg
custom customSend txParams =
    CustomSend customSend txParams


execute : Maybe (TxSentry.TxSentry msg) -> ChainCmd msg -> ( Maybe (TxSentry.TxSentry msg), Cmd msg, List (UN.UserNotice msg) )
execute maybeTxSentry chainCmdOrder =
    case ( maybeTxSentry, chainCmdOrder ) of
        ( _, None ) ->
            ( maybeTxSentry, Cmd.none, [] )

        ( Just txSentry, CustomSend customSend txParams ) ->
            TxSentry.customSend txSentry customSend txParams
                |> (\( a, b ) ->
                        ( Just a, b, [] )
                   )

        _ ->
            ( Nothing
            , Cmd.none
            , [ UN.unexpectedError
                    "submodel sent chainCmd, but there is no txSentry to use!"
                    Nothing
              ]
            )


map : (subMsg -> msg) -> ChainCmd subMsg -> ChainCmd msg
map f chainCmdOrder =
    case chainCmdOrder of
        None ->
            None

        CustomSend customSend txParams ->
            let
                newCustomSend =
                    TxSentry.CustomSend
                        (Maybe.map ((<<) f) customSend.onSign)
                        (Maybe.map ((<<) f) customSend.onBroadcast)
                        (Maybe.map
                            (\( subMsg1, trackerConfig ) ->
                                ( subMsg1 >> f
                                , Maybe.map
                                    (\tracker -> { confirmations = tracker.confirmations, toMsg = tracker.toMsg >> f })
                                    trackerConfig
                                )
                            )
                            customSend.onMined
                        )
            in
            CustomSend newCustomSend txParams

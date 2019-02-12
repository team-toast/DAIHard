module ChainCmd exposing (ChainCmd, custom, execute, map, none)

import Eth.Sentry.Tx as TxSentry
import Eth.Types


type ChainCmd msg
    = CustomSend (TxSentry.CustomSend msg) Eth.Types.Send
    | None


none : ChainCmd msg
none =
    None


custom : TxSentry.CustomSend msg -> Eth.Types.Send -> ChainCmd msg
custom customSend txParams =
    CustomSend customSend txParams


execute : TxSentry.TxSentry msg -> ChainCmd msg -> ( TxSentry.TxSentry msg, Cmd msg )
execute txSentry chainCmdOrder =
    case chainCmdOrder of
        None ->
            ( txSentry, Cmd.none )

        CustomSend customSend txParams ->
            TxSentry.customSend txSentry customSend txParams


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

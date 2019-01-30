module ChainCmd exposing (ChainCmdOrder, custom, execute, map, none)

import Eth.Sentry.Tx as TxSentry
import Eth.Types


type ChainCmdOrder msg
    = CustomSend (TxSentry.CustomSend msg) Eth.Types.Send
    | None


none : ChainCmdOrder msg
none =
    None


custom : TxSentry.CustomSend msg -> Eth.Types.Send -> ChainCmdOrder msg
custom customSend txParams =
    CustomSend customSend txParams


execute : TxSentry.TxSentry msg -> ChainCmdOrder msg -> ( TxSentry.TxSentry msg, Cmd msg )
execute txSentry chainCmdOrder =
    case chainCmdOrder of
        None ->
            ( txSentry, Cmd.none )

        CustomSend customSend txParams ->
            TxSentry.customSend txSentry customSend txParams


map : (subMsg -> msg) -> ChainCmdOrder subMsg -> ChainCmdOrder msg
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

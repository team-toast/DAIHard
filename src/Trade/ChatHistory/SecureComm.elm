module Trade.ChatHistory.SecureComm exposing (FullCommInfo, PartialCommInfo, SecureCommInfo(..), checkIfCommInfoLoaded, decodeDecryptionResult, decodeEncryptedMessage, decodeEncryptionResult, decodeSizedStringHelper, encodeDecryptionArgs, encodeEncryptedMessage, encodeEncryptionArgs, encodeSizedStrings, partialCommInfo, updateInitiatorPubkey, updateResponderPubkey)

import Array exposing (Array)
import CommonTypes exposing (..)
import Json.Decode
import Json.Encode
import Result.Extra
import Time
import Trade.ChatHistory.Types exposing (..)


type SecureCommInfo
    = PartiallyLoadedCommInfo PartialCommInfo
    | LoadedCommInfo FullCommInfo


type alias PartialCommInfo =
    { initiatorPubkey : Maybe String
    , responderPubkey : Maybe String
    }


type alias FullCommInfo =
    { initiatorPubkey : String
    , responderPubkey : String
    }


partialCommInfo : SecureCommInfo
partialCommInfo =
    PartiallyLoadedCommInfo <| PartialCommInfo Nothing Nothing


updateResponderPubkey : String -> SecureCommInfo -> SecureCommInfo
updateResponderPubkey pubkey commInfo =
    case commInfo of
        PartiallyLoadedCommInfo pInfo ->
            { pInfo | responderPubkey = Just pubkey }
                |> checkIfCommInfoLoaded

        LoadedCommInfo info ->
            LoadedCommInfo { info | responderPubkey = pubkey }


updateInitiatorPubkey : String -> SecureCommInfo -> SecureCommInfo
updateInitiatorPubkey pubkey commInfo =
    case commInfo of
        PartiallyLoadedCommInfo pInfo ->
            { pInfo | initiatorPubkey = Just pubkey }
                |> checkIfCommInfoLoaded

        LoadedCommInfo info ->
            LoadedCommInfo { info | initiatorPubkey = pubkey }


checkIfCommInfoLoaded : PartialCommInfo -> SecureCommInfo
checkIfCommInfoLoaded pInfo =
    case ( pInfo.initiatorPubkey, pInfo.responderPubkey ) of
        ( Just initiatorPubkey, Just responderPubkey ) ->
            LoadedCommInfo <|
                FullCommInfo
                    initiatorPubkey
                    responderPubkey

        _ ->
            PartiallyLoadedCommInfo pInfo


encodeEncryptionArgs : String -> FullCommInfo -> Json.Encode.Value
encodeEncryptionArgs message commInfo =
    Json.Encode.object
        [ ( "message", Json.Encode.string message )
        , ( "pubkeyHexStrings"
          , Json.Encode.list Json.Encode.string
                [ commInfo.initiatorPubkey
                , commInfo.responderPubkey
                ]
          )
        ]


encodeDecryptionArgs : Int -> EncryptedMessage -> Json.Encode.Value
encodeDecryptionArgs messageID encryptedMessage =
    Json.Encode.object
        [ ( "id", Json.Encode.int messageID )
        , ( "encapsulation", Json.Encode.string encryptedMessage.encapsulatedKey )
        , ( "iv", Json.Encode.string encryptedMessage.iv )
        , ( "tag", Json.Encode.string encryptedMessage.tag )
        , ( "encrypted", Json.Encode.string encryptedMessage.message )
        ]


decodeEncryptionResult : Json.Decode.Value -> Result String ( EncryptedMessage, EncryptedMessage )
decodeEncryptionResult value =
    let
        encryptedMessageDecoder =
            Json.Decode.map4 EncryptedMessage
                (Json.Decode.field "encapsulation" Json.Decode.string)
                (Json.Decode.field "iv" Json.Decode.string)
                (Json.Decode.field "tag" Json.Decode.string)
                (Json.Decode.field "encrypted" Json.Decode.string)

        decoder =
            Json.Decode.list encryptedMessageDecoder
    in
    case Json.Decode.decodeValue decoder value of
        Err decodeErr ->
            Err (Json.Decode.errorToString decodeErr)

        Ok list ->
            list
                |> Array.fromList
                |> (\arr ->
                        case ( Array.get 0 arr, Array.get 1 arr ) of
                            ( Just initiatorMessage, Just responderMessage ) ->
                                Ok ( initiatorMessage, responderMessage )

                            _ ->
                                Err "Decoded list has less than 2 items."
                   )


decodeDecryptionResult : Json.Decode.Value -> Result String ( Int, String )
decodeDecryptionResult value =
    let
        decoder =
            Json.Decode.map2
                Tuple.pair
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "message" Json.Decode.string)
    in
    Json.Decode.decodeValue decoder value
        |> Result.mapError Json.Decode.errorToString


encodeEncryptedMessage : EncryptedMessage -> Result String String
encodeEncryptedMessage encryptedMessage =
    encodeSizedStrings
        [ encryptedMessage.encapsulatedKey, encryptedMessage.iv, encryptedMessage.tag, encryptedMessage.message ]


encodeSizedStrings : List String -> Result String String
encodeSizedStrings strings =
    let
        prependWithLengthAsChar s =
            let
                len =
                    String.length s
            in
            if len > 0x0010FFFF then
                -- Char.fromCode / Char.toCode encoding hack breaks past this limit
                Err "string is too long"

            else
                Ok <|
                    String.cons (Char.fromCode len) s
    in
    strings
        |> List.map prependWithLengthAsChar
        |> Result.Extra.combine
        |> Result.map (String.join "")


decodeEncryptedMessage : String -> Maybe EncryptedMessage
decodeEncryptedMessage encoded =
    let
        stringArray =
            decodeSizedStringHelper (String.toList encoded) []
                |> Array.fromList
    in
    Maybe.map4
        EncryptedMessage
        (Array.get 0 stringArray)
        (Array.get 1 stringArray)
        (Array.get 2 stringArray)
        (Array.get 3 stringArray)


decodeSizedStringHelper : List Char -> List String -> List String
decodeSizedStringHelper remaining processed =
    case remaining of
        [] ->
            processed

        c :: r ->
            let
                len =
                    Char.toCode c

                str =
                    List.take len r
                        |> String.fromList

                newRemaining =
                    List.drop len r

                newProcessed =
                    processed ++ [ str ]
            in
            decodeSizedStringHelper newRemaining newProcessed

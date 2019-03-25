module PaymentMethods exposing (BankIdentifier, BankIdentifierType(..), PaymentMethod(..), bankIdentifierDecoder, bankIdentifierTypeDecoder, decodePaymentMethodList, decoder, demoView, encode, encodeBankIdentifier, encodeBankIdentifierType)

import Element
import Element.Border
import Json.Decode
import Json.Encode
import Json.Encode.Extra
import Maybe.Extra as Maybe


type PaymentMethod
    = Custom String
    | CashDrop String
    | CashHandoff String
    | BankTransfer BankIdentifier


encode : PaymentMethod -> Json.Encode.Value
encode paymentMethod =
    let
        ( transferTypeString, transferInfo ) =
            case paymentMethod of
                Custom s ->
                    ( "custom"
                    , Json.Encode.string s
                    )

                CashDrop s ->
                    ( "cashdrop"
                    , Json.Encode.string s
                    )

                CashHandoff s ->
                    ( "handoff"
                    , Json.Encode.string s
                    )

                BankTransfer identifier ->
                    ( "banktx"
                    , encodeBankIdentifier identifier
                    )
    in
    Json.Encode.object
        [ ( "type", Json.Encode.string transferTypeString )
        , ( "info", transferInfo )
        ]


decoder : Json.Decode.Decoder PaymentMethod
decoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\typeStr ->
                case typeStr of
                    "custom" ->
                        Json.Decode.map Custom <|
                            Json.Decode.field "info" Json.Decode.string

                    "cashdrop" ->
                        Json.Decode.map CashDrop <|
                            Json.Decode.field "info" Json.Decode.string

                    "handoff" ->
                        Json.Decode.map CashHandoff <|
                            Json.Decode.field "info" Json.Decode.string

                    "banktx" ->
                        Json.Decode.map BankTransfer <|
                            Json.Decode.field "info" bankIdentifierDecoder

                    unrecognizedType ->
                        Json.Decode.fail <|
                            "unrecognized transfer type: "
                                ++ unrecognizedType
            )


decodePaymentMethodList : String -> Result String (List PaymentMethod)
decodePaymentMethodList s =
    Json.Decode.decodeString (Json.Decode.list decoder) s
        -- If the decode fails, we keep the undecodeable string to display to the user later
        |> Result.mapError
            (\_ -> s)


type alias BankIdentifier =
    { identifierType : BankIdentifierType
    , info : String
    }


encodeBankIdentifier : BankIdentifier -> Json.Encode.Value
encodeBankIdentifier identifier =
    let
        typeValue =
            encodeBankIdentifierType identifier.identifierType
    in
    Json.Encode.object
        [ ( "type", typeValue )
        , ( "info", Json.Encode.string identifier.info )
        ]


bankIdentifierDecoder : Json.Decode.Decoder BankIdentifier
bankIdentifierDecoder =
    Json.Decode.map2
        BankIdentifier
        (Json.Decode.field "type" bankIdentifierTypeDecoder)
        (Json.Decode.field "info" Json.Decode.string)


type BankIdentifierType
    = ABA
    | SORT
    | IBAN
    | SWIFT
    | Name


encodeBankIdentifierType : BankIdentifierType -> Json.Encode.Value
encodeBankIdentifierType t =
    Json.Encode.string <|
        case t of
            ABA ->
                "ABA"

            SORT ->
                "SORT"

            IBAN ->
                "IBAN"

            SWIFT ->
                "SWIFT"

            Name ->
                "name"


bankIdentifierTypeDecoder : Json.Decode.Decoder BankIdentifierType
bankIdentifierTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\typeStr ->
                case typeStr of
                    "ABA" ->
                        Json.Decode.succeed ABA

                    "SORT" ->
                        Json.Decode.succeed SORT

                    "IBAN" ->
                        Json.Decode.succeed IBAN

                    "SWIFT" ->
                        Json.Decode.succeed SWIFT

                    "name" ->
                        Json.Decode.succeed Name

                    unrecognizedType ->
                        Json.Decode.fail <|
                            "unrecognized transfer type: "
                                ++ unrecognizedType
            )


demoView : PaymentMethod -> Element.Element msg
demoView paymentMethod =
    Element.column [ Element.padding 5, Element.spacing 5, Element.Border.width 1, Element.Border.rounded 5 ] <|
        Maybe.values <|
            case paymentMethod of
                CashDrop s ->
                    [ Just <|
                        Element.text <|
                            "Cash Drop: "
                                ++ s
                    ]

                CashHandoff s ->
                    [ Just <|
                        Element.text <|
                            "Cash Handoff: "
                                ++ s
                    ]

                BankTransfer bankIdentifier ->
                    let
                        typeString =
                            case bankIdentifier.identifierType of
                                ABA ->
                                    "ABA"

                                SORT ->
                                    "SORT"

                                IBAN ->
                                    "IBAN"

                                SWIFT ->
                                    "SWIFT"

                                Name ->
                                    "name"
                    in
                    [ Just <|
                        Element.text <|
                            "Bank transfer ("
                                ++ bankIdentifier.info
                                ++ ")"
                    ]

                Custom s ->
                    [ Just <|
                        Element.text <|
                            "Custom: "
                                ++ s
                    ]

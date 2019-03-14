module PaymentMethods exposing (BankIdentifier, BankIdentifierType(..), CashDropInfo, CashHandoffInfo, Location, PaymentMethod(..), bankIdentifierDecoder, bankIdentifierTypeDecoder, cashDropInfoDecoder, cashHandoffInfoDecoder, demoView, encodeBankIdentifier, encodeBankIdentifierType, encodeCashDropInfo, encodeCashHandoffInfo, encodeLocation, encodePaymentMethod, locationDecoder, paymentMethodDecoder)

import Element
import Element.Border
import Json.Decode
import Json.Encode
import Json.Encode.Extra
import Maybe.Extra as Maybe


type PaymentMethod
    = Custom String
    | CashDrop CashDropInfo
    | CashHandoff CashHandoffInfo
    | BankTransfer BankIdentifier


encodePaymentMethod : PaymentMethod -> Json.Encode.Value
encodePaymentMethod paymentMethod =
    let
        ( transferTypeString, transferInfo ) =
            case paymentMethod of
                Custom s ->
                    ( "custom"
                    , Json.Encode.string s
                    )

                CashDrop info ->
                    ( "cashdrop"
                    , encodeCashDropInfo info
                    )

                CashHandoff info ->
                    ( "handoff"
                    , encodeCashHandoffInfo info
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


paymentMethodDecoder : Json.Decode.Decoder PaymentMethod
paymentMethodDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\typeStr ->
                case typeStr of
                    "custom" ->
                        Json.Decode.map Custom <|
                            Json.Decode.field "info" Json.Decode.string

                    "cashdrop" ->
                        Json.Decode.map CashDrop <|
                            Json.Decode.field "info" cashDropInfoDecoder

                    "handoff" ->
                        Json.Decode.map CashHandoff <|
                            Json.Decode.field "info" cashHandoffInfoDecoder

                    "banktx" ->
                        Json.Decode.map BankTransfer <|
                            Json.Decode.field "info" bankIdentifierDecoder

                    unrecognizedType ->
                        Json.Decode.fail <|
                            "unrecognized transfer type: "
                                ++ unrecognizedType
            )


type alias CashDropInfo =
    { location : Location
    , radius : Float
    , description : Maybe String
    }


encodeCashDropInfo : CashDropInfo -> Json.Encode.Value
encodeCashDropInfo info =
    Json.Encode.object
        [ ( "location", encodeLocation info.location )
        , ( "radius", Json.Encode.float info.radius )
        , ( "description", Json.Encode.Extra.maybe Json.Encode.string info.description )
        ]


cashDropInfoDecoder : Json.Decode.Decoder CashDropInfo
cashDropInfoDecoder =
    Json.Decode.map3
        CashDropInfo
        (Json.Decode.field "location" locationDecoder)
        (Json.Decode.field "radius" Json.Decode.float)
        (Json.Decode.field "description" (Json.Decode.nullable Json.Decode.string))


type alias CashHandoffInfo =
    { location : Location
    , radius : Float
    , description : Maybe String
    }


encodeCashHandoffInfo : CashHandoffInfo -> Json.Encode.Value
encodeCashHandoffInfo info =
    Json.Encode.object
        [ ( "location", encodeLocation info.location )
        , ( "radius", Json.Encode.float info.radius )
        , ( "description", Json.Encode.Extra.maybe Json.Encode.string info.description )
        ]


cashHandoffInfoDecoder : Json.Decode.Decoder CashHandoffInfo
cashHandoffInfoDecoder =
    Json.Decode.map3
        CashDropInfo
        (Json.Decode.field "location" locationDecoder)
        (Json.Decode.field "radius" Json.Decode.float)
        (Json.Decode.field "description" (Json.Decode.nullable Json.Decode.string))


type alias Location =
    { lat : Float
    , long : Float
    }


encodeLocation : Location -> Json.Encode.Value
encodeLocation location =
    Json.Encode.list Json.Encode.float
        [ location.lat
        , location.long
        ]


locationDecoder : Json.Decode.Decoder Location
locationDecoder =
    Json.Decode.map2
        Location
        (Json.Decode.index 0 Json.Decode.float)
        (Json.Decode.index 1 Json.Decode.float)


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
                CashDrop info ->
                    [ Just <|
                        Element.text <|
                            "cash drop within "
                                ++ String.fromFloat info.radius
                                ++ " km of "
                                ++ locationToString info.location
                    , info.description
                        |> Maybe.map
                            (\desc ->
                                Element.text <| "description: " ++ desc
                            )
                    ]

                CashHandoff info ->
                    [ Just <|
                        Element.text <|
                            "cash handoff within "
                                ++ String.fromFloat info.radius
                                ++ " km of "
                                ++ locationToString info.location
                    , info.description
                        |> Maybe.map
                            (\desc ->
                                Element.text <| "description: " ++ desc
                            )
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


locationToString : Location -> String
locationToString location =
    "(lat:"
        ++ String.fromFloat location.lat
        ++ ",long:"
        ++ String.fromFloat location.long
        ++ ")"

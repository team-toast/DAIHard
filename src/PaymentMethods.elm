module PaymentMethods exposing
    ( PaymentMethod
    , Type(..)
    , blank
    , decodePaymentMethodList
    , decoder
    , encode
    , stringToType
    , typeDecoder
    , typeToString
    )

import Element
import Element.Border
import Json.Decode
import Json.Encode
import Json.Encode.Extra
import Maybe.Extra as Maybe


type alias PaymentMethod =
    { type_ : Type
    , info : String
    }


type Type
    = Cash
    | Bank
    | Custom


blank : Type -> PaymentMethod
blank type_ =
    PaymentMethod
        type_
        ""


typeToString : Type -> String
typeToString t =
    case t of
        Cash ->
            "cash"

        Bank ->
            "bank"

        Custom ->
            "custom"


stringToType : String -> Maybe Type
stringToType s =
    case s of
        "cash" ->
            Just Cash

        "bank" ->
            Just Bank

        "custom" ->
            Just Custom

        _ ->
            Nothing


encode : PaymentMethod -> Json.Encode.Value
encode paymentMethod =
    Json.Encode.object
        [ ( "type", Json.Encode.string (typeToString paymentMethod.type_) )
        , ( "info", Json.Encode.string paymentMethod.info )
        ]


decoder : Json.Decode.Decoder PaymentMethod
decoder =
    Json.Decode.map2
        PaymentMethod
        (Json.Decode.field "type" typeDecoder)
        (Json.Decode.field "info" Json.Decode.string)


typeDecoder : Json.Decode.Decoder Type
typeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case stringToType s of
                    Just t ->
                        Json.Decode.succeed t

                    Nothing ->
                        Json.Decode.fail ""
            )


decodePaymentMethodList : String -> Result String (List PaymentMethod)
decodePaymentMethodList s =
    Json.Decode.decodeString (Json.Decode.list decoder) s
        -- If the decode fails, we keep the undecodeable string to display to the user later
        |> Result.mapError
            (\_ -> s)

module PaymentMethods exposing
    ( PaymentMethod
    , Type(..)
    , blank
    , decodePaymentMethodList
    , decoder
    , encode
    , getTitle
    , stringToType
    , typeDecoder
    , typeToIcon
    , typeToString
    , viewList
    )

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import ElementHelpers as EH
import Images exposing (Image)
import Json.Decode
import Json.Encode
import Json.Encode.Extra
import List.Extra
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


getTitle : Type -> String
getTitle type_ =
    case type_ of
        Cash ->
            "Cash Drop/Handoff"

        Bank ->
            "Bank Transfer"

        Custom ->
            "Custom Payment Method"


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


decodePaymentMethodList : String -> Result Json.Decode.Error (List PaymentMethod)
decodePaymentMethodList s =
    Json.Decode.decodeString (Json.Decode.list decoder) s


viewList : List PaymentMethod -> Maybe msg -> Element msg
viewList pmList maybeAddMsg =
    Element.column
        [ Element.spacing 40
        , Element.width Element.fill
        ]
        [ paymentMethodsHeaderElement (List.length pmList) maybeAddMsg
        , paymentMethodsElement pmList
        ]


paymentMethodsHeaderElement : Int -> Maybe msg -> Element msg
paymentMethodsHeaderElement numMethods maybeAddMsg =
    let
        titleStr =
            "Accepted Payment Methods ("
                ++ String.fromInt numMethods
                ++ ")"

        addMethodButton =
            case maybeAddMsg of
                Just msg ->
                    Images.toElement
                        [ Element.pointer
                        , Element.Events.onClick msg
                        ]
                        Images.addButton

                Nothing ->
                    Element.none
    in
    Element.row
        [ Element.spacing 8
        , Element.paddingXY 40 0
        , Element.Font.size 23
        , Element.Font.semiBold
        ]
        [ Element.text titleStr
        , addMethodButton
        ]


paymentMethodsElement : List PaymentMethod -> Element msg
paymentMethodsElement pmList =
    Element.el
        [ Element.paddingXY 40 0
        , Element.width Element.fill
        ]
        (pmList
            |> List.map pmElement
            |> doubleColumn
        )


pmElement : PaymentMethod -> Element msg
pmElement pm =
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.shrink |> Element.maximum 300)
        , Element.spacing 1
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height Element.shrink
            , Element.paddingXY 60 40
            , EH.roundTopCorners 8
            , Element.Background.color EH.white
            , Element.Font.size 22
            , Element.Font.semiBold
            ]
            (Element.text <| getTitle pm.type_)
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.shrink
            , Element.paddingXY 60 40
            , EH.roundBottomCorners 8
            , Element.Background.color EH.white
            ]
            (Element.paragraph
                [ Element.Font.size 17
                , Element.Font.medium
                ]
                [ Element.text pm.info ]
            )
        ]


doubleColumn : List (Element msg) -> Element msg
doubleColumn elList =
    elList
        |> List.Extra.greedyGroupsOf 2
        |> List.map
            (\row ->
                if List.length row == 1 then
                    List.append row [ Element.none ]

                else
                    row
            )
        |> List.map
            (\row ->
                Element.row
                    [ Element.spacing 30
                    , Element.width Element.fill
                    ]
                    (row
                        |> List.map (Element.el [ Element.width Element.fill ])
                    )
            )
        |> Element.column
            [ Element.spacing 30
            , Element.width Element.fill
            ]


typeToIcon : Type -> Image
typeToIcon pmType =
    case pmType of
        Cash ->
            Images.pmCash

        Bank ->
            Images.pmBank

        Custom ->
            Images.pmCustom

module UserNotice exposing (..)

import Element exposing (Attribute, Element)
import Element.Font
import Http
import MaybeDebugLog exposing (maybeDebugLog)


type alias UserNotice msg =
    { noticeType : NoticeType
    , mainParagraphs : List (List (Element msg))
    , align : Alignment
    , label : String
    }


type Alignment
    = BottomRight
    | TopLeft


map : (msg1 -> msg2) -> UserNotice msg1 -> UserNotice msg2
map f userNotice =
    UserNotice
        userNotice.noticeType
        (userNotice.mainParagraphs
            |> List.map (List.map (Element.map f))
        )
        userNotice.align
        userNotice.label


type NoticeType
    = Update
    | Caution
    | Error
    | ShouldBeImpossible


placeholderNotice s =
    UserNotice
        Caution
        [ [ Element.text s ] ]


screenToSmall : Int -> UserNotice msg
screenToSmall width =
    { label = "screenToSmall"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Your screen is quite small (" ++ String.fromInt width ++ "--things may be very broken! We will be addressing this soon." ] ]
    , align = TopLeft
    }


invalidUrl : UserNotice msg
invalidUrl =
    { label = "invalidUrl"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "I don't understand that URL..." ]
        , [ Element.text "I'll just set you down here. Maybe check the URL and try again?" ]
        ]
    , align = BottomRight
    }


cantFetchPrices : UserNotice msg
cantFetchPrices =
    { label = "cantFetchPrices"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "Error encountered fetching crypto prices." ] ]
    , align = BottomRight
    }


noWeb3Provider : UserNotice msg
noWeb3Provider =
    { label = "noWeb3Provider"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "No web3 provider detected. Is "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "Metamask"
                }
          , Element.text " or some other web3 provider installed and unlocked?"
          ]
        ]
    , align = BottomRight
    }


cantConnectNoWeb3 : UserNotice msg
cantConnectNoWeb3 =
    { label = "cantConnectNoWeb3"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "You need a web3 provider (such as "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "Metamask"
                }
          , Element.text ") to Connect."
          ]
        , [ Element.text "Until you connect, DAIHard will operate in read-only mode." ]
        ]
    , align = BottomRight
    }


noWeb3Account : UserNotice msg
noWeb3Account =
    { label = "noWeb3Account"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "I can't detect a web3 account. Your wallet may be locked." ]
        ]
    , align = BottomRight
    }


wrongWeb3Network : UserNotice msg
wrongWeb3Network =
    { label = "wrongWeb3Network"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Make sure your wallet is set to a compatible network." ]
        , [ Element.text " At the moment DAIHard works on: xDai, the Kovan Ethereum Test Net, or Ethereum Main Net." ]
        ]
    , align = BottomRight
    }


unexpectedError : String -> a -> UserNotice msg
unexpectedError text debugObj =
    let
        _ =
            maybeDebugLog text debugObj
    in
    { label = "unexpectedError"
    , noticeType = ShouldBeImpossible
    , mainParagraphs = [ [ Element.text text ] ]
    , align = BottomRight
    }


web3FetchError : String -> Http.Error -> UserNotice msg
web3FetchError label httpError =
    let
        _ =
            maybeDebugLog (label ++ " fetch error") httpError
    in
    { label = "web3FetchError"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <|
                "Failed to fetch \""
                    ++ label
                    ++ "\"."
          ]
        ]
    , align = BottomRight
    }


web3SigError : String -> String -> UserNotice msg
web3SigError label errStr =
    { label = "web3SigError"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Error signing \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


web3MiningError : String -> String -> UserNotice msg
web3MiningError label errStr =
    { label = "web3MiningError"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error mining \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


cantFindTradeWillRetry : UserNotice msg
cantFindTradeWillRetry =
    { label = "cantFindTradeWillRetry"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Can't find a trade by that ID. I'll try again in half a second." ] ]
    , align = BottomRight
    }


fromBadFetchResultMaybe : String -> Result Http.Error (Maybe a) -> UserNotice msg
fromBadFetchResultMaybe label badFetchResultMaybe =
    case badFetchResultMaybe of
        Err httpErr ->
            web3FetchError label httpErr

        Ok Nothing ->
            unexpectedError
                ("Couldn't decode the fetched \"" ++ label ++ "\" result.")
                Nothing

        wut ->
            unexpectedError
                "fromBadFetchResultMaybe being called with an 'OK (Just a)' value"
                wut


walletError : String -> UserNotice msg
walletError errStr =
    unexpectedError
        ("Error decoding JS walletSentry: " ++ errStr)
        Nothing


inputError : String -> UserNotice msg
inputError errStr =
    { label = "inputError"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text errStr ] ]
    , align = BottomRight
    }


tradeParametersNotDefault : UserNotice msg
tradeParametersNotDefault =
    { label = "tradeParametersNotDefault"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Sorry, I'm not yet able to deal with this kind of trade. This must have been created by some other DAIHard interface." ] ]
    , align = BottomRight
    }


oldPriceDataWarning : UserNotice msg
oldPriceDataWarning =
    { label = "oldPriceDataWarning"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "Uh oh, it looks like our price data might be out of date. You might want to double check the math..." ] ]
    , align = BottomRight
    }


debugMsg : String -> UserNotice msg
debugMsg s =
    { label = "debug"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "debug: " ++ s ] ]
    , align = BottomRight
    }

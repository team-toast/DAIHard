module UserNotice exposing (NoticeType(..), UserNotice, cantConnectNoWeb3, cantFindTradeWillRetry, fromBadFetchResultMaybe, inputError, invalidUrl, map, noWeb3Provider, placeholderNotice, screenToSmall, tradeParametersNotDefault, unexpectedError, walletError, web3FetchError, web3MiningError, web3SigError, wrongWeb3Network)

import Element exposing (Element)
import Element.Font
import Http


type alias UserNotice msg =
    { noticeType : NoticeType
    , mainParagraphs : List (List (Element msg))
    }


map : (msg1 -> msg2) -> UserNotice msg1 -> UserNotice msg2
map f userNotice =
    UserNotice
        userNotice.noticeType
        (userNotice.mainParagraphs
            |> List.map (List.map (Element.map f))
        )


type NoticeType
    = Update
    | Caution
    | Error
    | ShouldBeImpossible


placeholderNotice s =
    UserNotice
        Caution
        [ [ Element.text s ] ]


screenToSmall : UserNotice msg
screenToSmall =
    { noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "Your screen is quite small--things may be very broken! We will be addressing this soon." ] ]
    }


invalidUrl : UserNotice msg
invalidUrl =
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text "I don't understand that URL..." ]
        , [ Element.text "I'll just set you down here. Maybe check the URL and try again?" ]
        ]
    }


noWeb3Provider : UserNotice msg
noWeb3Provider =
    { noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "No web3 provider detected. Is "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "Metamask"
                }
          , Element.text " or some other web3 provider installed and unlocked?"
          ]
        ]
    }


cantConnectNoWeb3 : UserNotice msg
cantConnectNoWeb3 =
    { noticeType = Caution
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
    }


wrongWeb3Network : UserNotice msg
wrongWeb3Network =
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Your wallet is set to an incorrect network." ]
        , [ Element.text "Switch to Ethereum mainnet, Ethereum test, Rootstock, or xDai." ]
        ]
    }


unexpectedError : String -> a -> UserNotice msg
unexpectedError text debugObj =
    let
        _ =
            Debug.log text debugObj
    in
    { noticeType = ShouldBeImpossible
    , mainParagraphs = [ [ Element.text text ] ]
    }


web3FetchError : String -> Http.Error -> UserNotice msg
web3FetchError label httpError =
    let
        _ =
            Debug.log (label ++ " fetch error") httpError
    in
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text <|
                "Failed to fetch \""
                    ++ label
                    ++ "\". See console output for more info."
          ]
        ]
    }


web3SigError : String -> String -> UserNotice msg
web3SigError label errStr =
    { noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Error signing \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    }


web3MiningError : String -> String -> UserNotice msg
web3MiningError label errStr =
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error mining \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    }


cantFindTradeWillRetry : UserNotice msg
cantFindTradeWillRetry =
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Can't find a trade by that ID. I'll try again in half a second." ] ]
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
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text errStr ] ]
    }


tradeParametersNotDefault : UserNotice msg
tradeParametersNotDefault =
    { noticeType = Error
    , mainParagraphs =
        [ [ Element.text "Sorry, I'm not yet able to deal with this kind of trade. This must have been created by some other DAIHard interface." ] ]
    }

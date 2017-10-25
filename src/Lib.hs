{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Lib
    ( webserver
    ) where

import Data.Monoid ((<>))
import Control.Monad
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import System.Environment (getEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (tlsManagerSettings, http, newManager, parseRequest, responseBody, Manager)
import Text.HTML.DOM (sinkDoc)
import Text.XML (Document)
import Data.Maybe (listToMaybe, maybe)
import Data.Text as T (Text, unwords)
import Data.Text.Lazy as L (fromStrict, unpack, pack)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (>=>), (&//))
import qualified NLP.RAKE.Text as RAKE (keywords, WordScore, sortByScore)


data Keywords = Keywords {keywords :: [Text]} deriving (Show, Generic)

instance ToJSON Keywords
instance FromJSON Keywords

webserver :: IO ()
webserver = do
    manager <- newManager tlsManagerSettings
    port <- read <$> getEnv "PORT"
    scotty port $ do
        get "/" $ do
            text "server is active"
        get "/url" $ do
            res <- (liftM (\t -> Just t) $ param "url") `rescue` (\_ -> return Nothing)
            case res of 
                Just url -> do
                    text url
                    document <- (liftM (\t -> Just t) $ liftAndCatchIO $ makeRequest manager (L.unpack url)) `rescue` (\_ -> return Nothing)
                    case document of
                        Just doc -> json $ Keywords $ getFiveBest $ getParagraph doc
                        Nothing -> return ()
                Nothing -> return ()



makeRequest :: Manager -> String -> IO Document
makeRequest manager url = do
    request <- parseRequest url
    runResourceT $ do
        response <- http request manager
        let body = responseBody response
        body $$+- sinkDoc

getParagraph :: Document -> Text
getParagraph document =  T.unwords contents where
    contents = cursor
        $// element "p"
        &// content
    cursor = fromDocument document


getAllParagraph :: Document ->  [Text]
getAllParagraph document = contents where
    contents = cursor
        $// element "p"
        &// content
    cursor = fromDocument document


getFiveBest :: Text -> [Text]
getFiveBest text = analyse text where
    analyse t = map fst $ take 5 $ RAKE.sortByScore $ RAKE.keywords t 






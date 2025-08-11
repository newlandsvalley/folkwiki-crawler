module FolkWikiPage
  ( getEditPage
  , retrieveAbcFromPage
  , retrieveTuneCount
  ) where

import Prelude

import Affjax as AX
import Affjax.Node as AN
import Affjax.ResponseFormat as RF
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String (indexOf, length, replaceAll, splitAt)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)


-- | Get the Wiki Edit page for the tune number in question
getEditPage :: Int -> Aff (Either Error String)
getEditPage n = do
  let 
    url = "http://folkwiki.se/Musik/" <> show n <> "?action=edit" 
  ePage <- AN.request (AX.defaultRequest { url=url, method = Left GET, responseFormat = RF.string })
  
  let 
    result = 
      case ePage of
        Left err -> 
          Left $ error $ "failed to retrieve " <> url <> " error: " <> AX.printError err
        Right response -> Right response.body
  
  pure result

-- | Extract the ABC from the first tune found on the page.  This is sandwiched between (:music:) and (:musicend:) delimiters
retrieveAbcFromPage :: Int -> Aff (Either Error String)
retrieveAbcFromPage n = do 
  eResult <- getEditPage n 
  case eResult of  
    Left e -> pure $ Left e 
    Right text -> do 
      let 
        mStart = indexOf (Pattern "(:music:)\n") text
        mEnd = indexOf (Pattern "(:musicend:)") text

      case mStart, mEnd of 
        Just start, Just end -> do
          let 
            { after, before} = splitAt end text
            result = splitAt (start + length "(:music:)\n") before
          pure $ Right $ fixEmbeddedQuotes $ result.after
        _, _ -> 
          pure $ Left $ error "no ABC delimiters found"


-- | Get the Wiki start page
getStartPage :: Aff (Either Error String)
getStartPage = do
  let 
    url = "http://folkwiki.se/Meta/Start" 
  ePage <- AN.request (AX.defaultRequest { url=url, method = Left GET, responseFormat = RF.string })
  
  let 
    result = 
      case ePage of
        Left err -> 
          Left $ error $ "failed to retrieve " <> url <> " error: " <> AX.printError err
        Right response -> Right response.body
  
  pure result

-- | Extract the tune count.  This is sandwiched between the words "totalt" and "låtar i wikin"
retrieveTuneCount :: Aff (Either Error Int)
retrieveTuneCount = do 
  eResult <- getStartPage
  case eResult of  
    Left e -> pure $ Left e 
    Right text -> do 
      let 
        mStart = indexOf (Pattern "totalt ") text
        mEnd = indexOf (Pattern " låtar i wikin") text

      case mStart, mEnd of 
        Just start, Just end -> do
          let 
            { after, before} = splitAt end text
            result = splitAt (start + length "totalt ") before
            mCount = fromString result.after
          case mCount of 
            Nothing -> 
              pure $ Left $ error "no legible tune count found"
            Just count ->
              pure $ Right $ count
        _, _ -> 
          pure $ Left $ error "no tune count found"


-- | Escape any embedded quotes in the raw text received from folkwiki
fixEmbeddedQuotes :: String -> String
fixEmbeddedQuotes = 
  replaceAll (Pattern "\"") (Replacement  "\\\"")
  




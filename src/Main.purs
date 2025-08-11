module Main where

import Prelude

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Traversable (for)
import Effect.Exception (message)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FolkWikiPage (retrieveAbcFromPage, retrieveTuneCount)
import Node.Path (FilePath)
import Node.Stream.Aff (end, fromStringUTF8, write)
import Node.FS.Stream (createWriteStream)
import Node.Stream (Writable)
import AbcParser (PropertyName(TuneId), addProperty, parse)
import JsonBuilder (buildJsonString)
import StringParser (printParserError)

main :: Effect Unit
main = void $ launchAff $ do
  eCount <- retrieveTuneCount
  case eCount of  
    Left _err -> do
      liftEffect $ log "tune count not found"
    Right count -> do
      liftEffect $ log $ "tune count: " <> (show count)
      
  let
    outfilename :: FilePath 
    -- outfilename = "data/folkwiki.txt" -- for writeAbc
    outfilename = "data/folkwiki.json"
  outfile <- liftEffect $ createWriteStream outfilename  
  -- let's just crawl through the first 10 for the time being
  _ <- for (1 .. 10) \n ->  do
    -- writeAbc n outfile
    writeJson n outfile
  end outfile
  _ <- liftEffect $ log "FolkWiki crawl finished"
  pure unit

writeAbc :: Int -> Writable () -> Aff Unit 
writeAbc n outfile = do
  eAbc <- retrieveAbcFromPage n
  case eAbc of  
    Left e -> do
      liftEffect $ log $ message e
    Right abc -> do
      write outfile =<< fromStringUTF8 abc 
  pure unit 

writeJson :: Int -> Writable () -> Aff Unit 
writeJson n outfile = do
  eAbc <- retrieveAbcFromPage n
  case eAbc of  
    Left e -> do
      liftEffect $ log $ message e
    Right abc -> do
      case (parse abc) of 
        Left parseError -> do
          liftEffect $ log $ printParserError parseError
        Right abcProperties -> do
          let 
            -- add the tune id number
            fullAbcProperties = addProperty TuneId (show n) abcProperties
            text = buildJsonString fullAbcProperties
          write outfile =<< fromStringUTF8 (text <> "\r\n")
  pure unit 




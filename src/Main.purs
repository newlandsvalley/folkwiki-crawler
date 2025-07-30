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
import FolkWikiPage (retrieveAbcFromPage)
import Node.Path (FilePath)
import Node.Stream.Aff (end, fromStringUTF8, write)
import Node.FS.Stream (createWriteStream)
import Node.Stream (Writable)



main :: Effect Unit
main = void $ launchAff $ do
  let
    outfilename :: FilePath 
    outfilename = "data/folkwiki.txt"
  outfile <- liftEffect $ createWriteStream outfilename  
  -- let's just crawl through the first 10 for the time being
  _ <- for (1 .. 10) \n ->  do
    writeAbc n outfile
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
  


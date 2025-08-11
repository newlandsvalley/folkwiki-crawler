module AbcParser
  ( AbcProperty
  , AbcProperties
  , PropertyName(..)
  , addProperty
  , parse
  )
  where

import Prelude (class Eq, class Ord, class Show, ($), (*>), (<*), (<*>), (<$>), (<$), (<<<), (==), show)

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.List (List(..), catMaybes, snoc)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import StringParser (Parser, ParseError, runParser, anyChar)
import StringParser.CodePoints (satisfy, string, char, eof, regex)
import StringParser.Combinators (choice, many, many1, manyTill, (<?>))

{-

example of a tune dumped in JSON format by theSession.org

{ "tune_id":"11931"
, "setting_id":"11931"
, "name":"'G Iomain Nan Gamhna"
, "type": "slip jig",
, "meter":"9/8"
, "mode":"Gmajor"
, "abc":"dBB B2 A BAG|dBB Bcd efg|dBB B2 A BAG|eAA dBG A2 e|\r\ndBB B2 A BAG|dBB Bcd efg|dBB B2 A BAG|eAA dBG A2 A|\r\nBAB g2 e fed|BAB e/f/g e f2 A|BAB g2 e fed|eAA ABd e2 A|\r\nBAB g2 e fed|BAB e/f/g e f2 f|def gfe fed|eAA ABd efg|"
, "date":"2012-05-17 07:49:26"
, "username":"iliketurtles"},

-}

-- | the properties that we will save which are the important headers and the ABC body itself
-- | plus also the TuneId which we have to add later
-- | Note that the first five of these are used in the lower levels of the parser, but we convert 
-- | to strings in the upper reaches because we eventually need to build a Foreign Object 
-- | which requires keys as Strings
data PropertyName = 
    Title       -- aka Name
  | Rhythm      -- aka Type
  | Meter 
  | Key         -- aka Mode
  | Abc         -- the ABC body
  | TuneId      -- the tune ID (not taken from the parse, but added later)

instance showPropertyName :: Show PropertyName where
  show Title = "name"
  show Rhythm = "type"
  show Meter = "meter"
  show Key = "mode"
  show Abc = "abc"
  show TuneId = "tune_id"

derive instance eqPropertyName :: Eq PropertyName
derive instance ordPropertyName :: Ord PropertyName

-- | An abc property is an important header or the abc body itself
-- | note that we have now moved the PropertyName type to a String
type AbcProperty =
  Tuple String String 

type AbcProperties = List AbcProperty 

addProperty :: PropertyName -> String -> AbcProperties -> AbcProperties 
addProperty name val props = 
  Cons (Tuple (show name) val) props

abcTune :: Parser AbcProperties
abcTune = 
  buildTune 
    <$> savedHeaders <*> body 

  where 

  buildTune headers abcBody = 
    snoc headers $ Tuple (show Abc) abcBody

savedHeaders :: Parser AbcProperties
savedHeaders = 
  (catMaybes <<< toList) <$> many1 (header <* eol)

body :: Parser String
body = 
  (fromCharArray <<< fromFoldable)
    <$> manyTill anyChar eof

header :: Parser (Maybe AbcProperty)
header =
  choice 
    [ title
    , rhythm
    , meter 
    , key
    , otherHeader 
    ]

title :: Parser (Maybe AbcProperty)
title  =
  makeHeader Title
    <$> ((string "T:") *> (whiteSpace *> restOfLine))
    <?> "Title header"

rhythm :: Parser (Maybe AbcProperty)
rhythm  =
  makeHeader Rhythm
    <$> ((string "R:") *> restOfLine)
    <?> "Rhythm header"

meter :: Parser (Maybe AbcProperty)
meter  =
  makeHeader Meter
    <$> ((string "M:") *> restOfLine)
    <?> "Meter header"

key :: Parser (Maybe AbcProperty)
key  =
  makeHeader Key
    <$> ((string "K:") *> restOfLine)
    <?> "Key header"

otherHeader :: Parser (Maybe AbcProperty)
otherHeader =
  makeNullHeader
    <$> (regex "[A-Za-z]:" *> restOfLine)
    <?> "other header"

makeHeader :: PropertyName -> String -> Maybe AbcProperty 
makeHeader name val = 
  Just $ Tuple (show name) val

makeNullHeader :: String -> Maybe AbcProperty 
makeNullHeader _val = 
  Nothing

restOfLine :: Parser String 
restOfLine = 
  strToEol

-- | parse a remaining string up to but not including the end of line
-- | here we intend to retain the string so we bar any comments
strToEol :: Parser String
strToEol =
  regex "[^\x0D\n%]*"


--| Parse an end of line character or sequence, returning a `\n` character. 
--| Before the actual end of line, we can have comments, which are discarded
eol :: Parser Char
eol =
  crlf <|> newline

-- | Parse a conventional carriage return, linefeedsequence, returning a `\n` character. 
-- | However, also accommodate the non-standard and deprecated exclamation mark which 
-- | is used to indicate line-breaks in older systems. See 10.2.1 Outdated line-breaking.
-- | Note that this brings ambiguity with respect to long decorations which are also introuduced
-- | by exclamation marks, but incorporating it into a regex means we don't need to protect
-- | it with a try, because the whole regex match is either consumed or not.
-- | Also accommodate a carriage return but without the terminating newline
crlf :: Parser Char
crlf = '\n' <$ regex "!?\r(\n)?" <?> "expected crlf"

newline :: Parser Char
newline = satisfy ((==) '\n') <?> "expected newline"

-- | our whiteSpace differs from that of the string parser we do NOT want to
-- | consume carriage returns or newlines
whiteSpace :: Parser String
whiteSpace =
  (fromCharArray <<< fromFoldable)
    <$> many scoreSpace

scoreSpace :: Parser Char
scoreSpace =
  -- tab <|> space
  (char '\t') <|> char ' '

-- | Parse an ABC tune image.
parse :: String -> Either ParseError AbcProperties
parse s =
  runParser abcTune s 

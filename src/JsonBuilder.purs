module JsonBuilder
  ( buildJsonString) where

import Prelude


import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.List (List)
import Data.Tuple (Tuple)
import Foreign.Object (Object, fromFoldableWith)
import AbcParser (AbcProperties)

-- | build a json String from the properties
buildJsonString :: AbcProperties -> String
buildJsonString = 
  stringify <<< encodeAbcProperties <<< buildObject 

  where 
  encodeAbcProperties :: Object String -> Json
  encodeAbcProperties =
    encodeJson

  -- | De-duplicate properties produced by the parser in favour of the first occurrence
  -- | and transform to a foreign object
  buildObject ∷ List (Tuple String String) -> Object String
  buildObject = 
    fromFoldableWith (flip const) 


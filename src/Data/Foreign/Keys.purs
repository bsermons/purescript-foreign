-- | This module provides functions for working with object properties
-- | of Javascript objects.

module Data.Foreign.Keys
  ( keys
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), typeOf, isUndefined, isNull)
import Data.NonEmpty (singleton)

foreign import unsafeKeys :: Foreign -> Array String

-- | Get an array of the properties defined on a foreign value
keys :: Foreign -> F (Array String)
keys value | isNull value = Left $ singleton (TypeMismatch ["object"] "null")
keys value | isUndefined value = Left $ singleton (TypeMismatch ["object"] "undefined")
keys value | typeOf value == "object" = Right $ unsafeKeys value
keys value = Left $ singleton (TypeMismatch ["object"] (typeOf value))

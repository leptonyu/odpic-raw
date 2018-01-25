module Database.Dpi.Prelude
  ( module Foreign
  , module Foreign.C.String
  , module Foreign.C.Types
  , module Database.Dpi.Prelude
  , packCString
  , packCStringLen
  , useAsCString
  , useAsCStringLen
  , ByteString
  , join
  , (<>)
  ) where

import           Control.Monad    (join)
import           Data.ByteString
import           Data.Monoid      ((<>))
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr p = if p == nullPtr then Nothing else Just p

fe :: (Enum e, Integral n) => e -> n
fe = fromIntegral . fromEnum

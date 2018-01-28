module Database.Dpi.Prelude
  ( module Foreign
  , module Foreign.C.String
  , module Foreign.C.Types
  , module Database.Dpi.Prelude
  , join
  , (<>)
  , Text
  ) where

import           Control.Monad    (join)
import           Data.Monoid      ((<>))
import           Data.Text
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr p = if p == nullPtr then Nothing else Just p

fe :: (Enum e, Integral n) => e -> n
fe = fromIntegral . fromEnum

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Dpi.Prelude
  ( module Foreign
  , module Foreign.C.String
  , module Foreign.C.Types
  , module Database.Dpi.Prelude
  , join
  , (<>)
  , (&)
  , HasCallStack
  , ByteString
  ) where

import           Control.Exception
import           Control.Monad     (join)
import           Data.ByteString   (ByteString)
import           Data.Function     ((&))
import           Data.Monoid       ((<>))
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Stack

{-# INLINE toMaybePtr #-}
toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr p = if p == nullPtr then Nothing else Just p

{-# INLINE fe #-}
fe :: (Enum e, Integral n) => e -> n
fe = fromIntegral . fromEnum

{-# INLINE te #-}
te :: (Integral n, Enum e) => n -> e
te = toEnum . fromIntegral

{-# INLINE noImplement #-}
noImplement :: HasCallStack => a
noImplement = error "Not supported yet"

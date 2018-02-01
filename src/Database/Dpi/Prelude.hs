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
  , Text
  , HasCallStack
  ) where

import           Control.Exception
import           Control.Monad     (join)
import           Data.Function     ((&))
import           Data.Monoid       ((<>))
import           Data.Text
import qualified Data.Text         as T
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

{-# INLINE ts #-}
ts :: Ptr CChar -> CUInt -> IO Text
ts p l | nullPtr == p = return ""
       | otherwise    = pack <$> peekCStringLen (p, fromIntegral l)

{-# INLINE tb #-}
tb :: Ptr CChar -> IO Text
tb p | nullPtr == p = return ""
     | otherwise    = pack <$> peekCString p

{-# INLINE fs #-}
fs :: Text -> IO CString
fs s | T.null s    = return nullPtr
     | otherwise = newCString $ unpack s

{-# INLINE fb #-}
fb :: Text -> IO CStringLen
fb s | T.null s    = return (nullPtr,0)
     | otherwise = newCStringLen $ unpack s

{-# INLINE noImplement #-}
noImplement :: HasCallStack => a
noImplement = error "Not supported yet"

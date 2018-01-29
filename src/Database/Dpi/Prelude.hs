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

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr p = if p == nullPtr then Nothing else Just p

fe :: (Enum e, Integral n) => e -> n
fe = fromIntegral . fromEnum

te :: (Integral n, Enum e) => n -> e
te = toEnum . fromIntegral

ts :: Ptr CChar -> CUInt -> IO Text
ts p l | nullPtr == p = return ""
       | otherwise    = pack <$> peekCStringLen (p, fromIntegral l)

tb :: Ptr CChar -> IO Text
tb p | nullPtr == p = return ""
     | otherwise    = pack <$> peekCString p

fs :: Text -> IO CString
fs s | T.null s    = return nullPtr
     | otherwise = newCString $ unpack s

fb :: Text -> IO CStringLen
fb s | T.null s    = return (nullPtr,0)
     | otherwise = newCStringLen $ unpack s

noImplement :: a
noImplement = error "Not supported yet"

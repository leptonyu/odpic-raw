{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}
module Database.Dpi.Util where

import           Database.Dpi.Internal
import           Database.Dpi.Prelude

import           Control.Exception
import qualified Data.Text             as T

isOk :: CInt -> Bool
isOk = (== success)

data DpiException
  = ErrorInfoException Data_ErrorInfo
  | DpiException Text
  | ConnectionPropNotFound Text
  deriving Show

instance Exception DpiException

throwDpiException :: Text -> IO a
throwDpiException = throw . DpiException

class WithPtrs a where
  withPtrs :: (a -> IO b) -> IO b

instance Storable a => WithPtrs (Ptr a) where
  withPtrs = alloca

instance (WithPtrs a, WithPtrs b) => WithPtrs (a, b) where
  withPtrs f = withPtrs $ \a -> withPtrs $ \b -> f (a,b)

class HasMonad m r | r -> m where
  app :: m a -> (a -> r) -> r
  unM :: m r -> r
  unM ma = app ma id

instance Monad m => HasMonad m (m a) where
  app = (>>=)

instance (HasMonad m r) => HasMonad m (a -> r) where
  app mb f = app mb . flip f

inStr :: (HasMonad IO r) => Text -> (CString -> r) -> r
inStr text f = unM $ withCString (T.unpack text) (return . f)

inStrLen :: (HasMonad IO r) => Text -> (Ptr CChar -> CUInt -> r) -> r
inStrLen text f = unM $ withCStringLen (T.unpack text) $ \(c,clen) -> return $ f c (fromIntegral clen)

inInt :: (Num n, Integral i) => i -> (n -> r) -> r
inInt n f = f $ fromIntegral n

inPtr :: (HasMonad IO r, Storable a) => (Ptr a -> IO b) -> (Ptr a -> r) -> r
inPtr init f = unM $ withPtrs $ \c -> init c >> return (f c)

outPtrs :: WithPtrs a => (a -> r -> IO b) -> (a -> IO r) -> IO b
outPtrs g f = withPtrs $ \a -> f a >>= g a

checkOk :: Storable a => Text -> (Ptr a -> IO b) -> Ptr a -> CInt -> IO b
checkOk err f p r = if isOk r then f p else throwDpiException err

runOk :: (Ptr a -> IO CInt) -> Ptr a -> IO Bool
runOk f p = isOk <$> f p

peekInt :: (Num n, Integral a, Storable a) => Ptr a -> IO n
peekInt p = fromIntegral <$> peek p

peekBool :: Ptr CInt -> IO Bool
peekBool p = isOk <$> peek p

_getConnValue :: (PtrConn -> Ptr (Ptr CChar) -> Ptr CUInt -> IO Text -> IO a) -> PtrConn -> IO a
_getConnValue f p = withPtrs $ \(ps,pslen) -> f p ps pslen (join $ ts <$> peek ps <*> peek pslen)

_getConn :: Text -> (PtrConn -> Ptr (Ptr CChar) -> Ptr CUInt -> IO CInt) -> PtrConn -> IO Text
_getConn key f = _getConnValue $ go key f
  where
    go key f p ps pslen pstr = do
      ok <- isOk <$> f p ps pslen
      if ok then pstr else throw $ ConnectionPropNotFound key

_getConnStrAndObj :: Storable a
                  => (PtrConn -> Ptr (Ptr CChar) -> Ptr CUInt -> Ptr a -> IO CInt)
                  -> Text
                  -> PtrConn
                  -> IO (Text, a)
_getConnStrAndObj f key = _getConnValue (go key f)
  where
    go key f p ps pslen pstr = alloca $ \pv -> do
      ok <- isOk <$> f p ps pslen pv
      if ok
        then do
          s <- pstr
          v <- peek pv
          return (s,v)
        else throw $ ConnectionPropNotFound key

_get' :: Storable a => DpiException -> (Ptr p -> Ptr a -> IO CInt) -> (Ptr a -> IO b) -> Ptr p -> IO b
_get' e f v p
  = alloca $ \pc -> do
      ok <- isOk <$> f p pc
      if ok then v pc else throw e


_getStmt :: Storable a => Text -> (PtrStmt -> Ptr a -> IO CInt) -> (Ptr a -> IO b) -> PtrStmt -> IO b
_getStmt c = _get' (DpiException c)

-- GetData
_get :: NativeTypeNum -> PtrData -> IO DataValue
_get t p = do
  Data get <- peek p
  get t

_objIndex :: (PtrObject -> Ptr CInt -> Ptr CInt -> IO CInt) -> PtrObject -> IO (Maybe Int)
_objIndex f p
  = alloca $ \pi ->
    alloca $ \pe -> do
      ok <- isOk <$> f p pi pe
      if ok
        then do
          e <- peekBool pe
          if e then (Just .fromIntegral) <$> peek pi else return Nothing
        else throwDpiException ""

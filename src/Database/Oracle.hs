{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Database.Oracle(
    DataColumn
  , DataRow
  , SQL
  , FromRow(..)
  , FromColumn(..)
  , queryByPage
  , queryAsRes
  , execute
  ) where

import           Database.Dpi

import           Control.Exception           (throw)
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.IO.Unlift     (MonadUnliftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Acquire                (Acquire, mkAcquire, with)
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.Int
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Word
import           Foreign.C.Types
import           GHC.Float

type DataColumn = (Text, Maybe Data_DataTypeInfo, Bool, DataValue)
type DataRow    = [DataColumn]
type SQL        = Text

class FromRow a where
  fromRow :: DataRow -> IO a

instance FromRow DataRow where
  {-# INLINE fromRow #-}
  fromRow = return

instance FromRow DataColumn where
  {-# INLINE fromRow #-}
  fromRow [v] = return v
  fromRow vs  = singleError vs "DataColumn"

class FromColumn a where
  fromColumn :: DataColumn -> IO (Maybe a)

{-# INLINE singleError #-}
singleError :: Show b => b -> Text -> IO a
singleError v name = throw $ DpiException $ T.pack (show v) <> " type mismatch to " <> name

instance FromColumn Text where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull    _) = return Nothing
  fromColumn (_,_,_,DataVarchar v) = return $ Just v
  fromColumn v                     = singleError v "Text"

instance FromColumn Int64 where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull _)  = return Nothing
  fromColumn (_,_,_,DataInt   v) = return $ Just v
  fromColumn v                   = singleError v "Int64"

instance FromColumn Word64 where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull _)   = return Nothing
  fromColumn (_,_,_,DataUint   v) = return $ Just v
  fromColumn v                    = singleError v "Word64"

instance FromColumn Float where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull _)             = return Nothing
  fromColumn (_,_,_,DataFloat  (CFloat  v)) = return $ Just v
  fromColumn (_,_,_,DataDouble (CDouble v)) = return $ Just $ double2Float v
  fromColumn v                              = singleError v "Float"

instance FromColumn Double where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull _)             = return Nothing
  fromColumn (_,_,_,DataFloat  (CFloat  v)) = return $ Just $ float2Double v
  fromColumn (_,_,_,DataDouble (CDouble v)) = return $ Just v
  fromColumn v                              = singleError v "Double"

instance FromColumn UTCTime where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull      _) = return Nothing
  fromColumn (_,_,_,DataTimestamp v) = return $ Just v
  fromColumn v                       = singleError v "UTCTime"

instance FromColumn DiffTime where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull      _)  = return Nothing
  fromColumn (_,_,_,DataIntervalDs v) = return $ Just v
  fromColumn v                        = singleError v "DiffTime"

instance FromColumn Data_IntervalYM where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull       _) = return Nothing
  fromColumn (_,_,_,DataIntervalYm v) = return $ Just v
  fromColumn v                        = singleError v "Data_IntervalYM"

instance FromColumn Bool where
  {-# INLINE fromColumn #-}
  fromColumn (_,_,_,DataNull    _) = return Nothing
  fromColumn (_,_,_,DataBoolean v) = return $ Just v
  fromColumn (_,_,_,DataInt     v) = return $ Just $ v /= 0
  fromColumn (_,_,_,DataUint    v) = return $ Just $ v /= 0
  fromColumn (_,_,_,DataFloat   v) = return $ Just $ v /= 0
  fromColumn (_,_,_,DataDouble  v) = return $ Just $ v /= 0
  fromColumn v                     = singleError v "Bool"

queryByPage :: (MonadUnliftIO m, MonadBaseControl IO m, FromRow a) => PtrConn -> SQL -> DataRow -> Page -> m [a]
queryByPage conn sql ps (offset,limit) = do
  let sql' = normalize sql <> " OFFSET " <> T.pack (show offset) <> " ROWS FETCH NEXT " <> T.pack (show limit) <> " ROWS ONLY"
  with (queryAsRes conn sql' ps) (\a -> runConduit $ a .| CL.fold (flip (:)) [])

queryAsRes :: (MonadIO m, FromRow a) => PtrConn -> SQL -> DataRow -> Acquire (ConduitT () a m ())
queryAsRes conn sql ps = do
  (st,ac) <- mkAcquire (pst conn sql ps) (Control.Monad.void . releaseStatement . fst)
  return $ pull st ac
  where
    {-# INLINE pst #-}
    pst conn sql ps = do
      st <- prepareStatement conn False (normalize sql)
      bindValue st ps
      r  <- executeStatement st ModeExecDefault
      return (st,r)
    {-# INLINE pull #-}
    pull st r = do
      mayC <- liftIO $ fetch st
      case mayC of
        Nothing  -> return ()
        (Just _) -> do
          vs <- liftIO $ mapM (getQueryValue st) [1..r]
          ps <- liftIO $ mapM (getQueryInfo  st) [1..r]
          cl <- liftIO $ fromRow $ zipWith meg ps vs
          yield cl
          pull st r
    {-# INLINE meg #-}
    meg Data_QueryInfo{..} v = (name, Just typeInfo, nullOk, v)

{-# INLINE bindValue #-}
bindValue :: PtrStmt -> DataRow -> IO ()
bindValue = mapM_ . bd
  where
    bd st (n,_,_,v) = bindValueByName st n v

execute :: MonadIO m => PtrConn -> SQL -> DataRow -> m Int
execute conn sql ps = liftIO $ do
  st <- prepareStatement conn False (normalize sql)
  bindValue st ps
  executeStatement st ModeExecDefault
  fromIntegral <$> getRowCount st


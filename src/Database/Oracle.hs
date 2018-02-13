{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Database.Oracle where

import           Database.Dpi

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Acquire                (Acquire, mkAcquire, with)
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T


type DataColumn = [(Text, Maybe OracleTypeNum, DataValue)]
type SQL        = Text

class FromDataColumn a where
  fromColumn :: DataColumn -> IO a

instance FromDataColumn DataColumn where
  fromColumn = return

queryByPage :: (MonadIO m, MonadBaseControl IO m, FromDataColumn a) => PtrConn -> SQL -> DataColumn -> Page -> m [a]
queryByPage conn sql ps (offset,limit) = do
  let sql' = normalize sql <> " OFFSET " <> T.pack (show offset) <> " ROWS FETCH NEXT " <> T.pack (show limit) <> " ROWS ONLY"
  with (query conn sql' ps) ($$ CL.fold (flip (:)) [])

query :: (MonadIO m, FromDataColumn a) => PtrConn -> SQL -> DataColumn -> Acquire (Source m a)
query conn sql ps = do
  st <- liftIO $ prepareStatement conn False (normalize sql)
  ac <- mkAcquire (bindValue st ps >> executeStatement st ModeExecDefault) (\_ -> Control.Monad.void $ releaseStatement st)
  return $ pull st ac
  where
    pull st r = do
      mayC <- liftIO $ fetch st
      case mayC of
        Nothing  -> return ()
        (Just _) -> do
          vs <- liftIO $ mapM (getQueryValue st) [1..r]
          ps <- liftIO $ mapM (getQueryInfo  st) [1..r]
          cl <- liftIO $ fromColumn $ zipWith meg ps vs
          yield cl
          pull st r
    meg Data_QueryInfo{..} v = let Data_DataTypeInfo{..} = typeInfo in (name, Just oracleTypeNum, v)

bindValue :: PtrStmt -> DataColumn -> IO ()
bindValue = mapM_ . bd
  where
    bd st (n,_,v) = bindValueByName st n v

execute :: MonadIO m => PtrConn -> SQL -> DataColumn -> m Int
execute conn sql ps = liftIO $ do
  st <- prepareStatement conn False (normalize sql)
  bindValue st ps
  executeStatement st ModeExecDefault
  fromIntegral <$> getRowCount st


{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Database.Dpi.Sql where

import           Database.Dpi
import           Database.Dpi.Field


import           Control.Monad      (when)
import           Data.Maybe
import           Data.Text          (Text, unpack)
import           System.Environment

getLanguage :: OracleConfig -> IO Text
getLanguage conf = withContext $ \cxt -> withConnection cxt conf id id $ \conn -> do
  withStatement conn False "SELECT USERENV ('language') FROM DUAL" $ \st -> do
    r     <- executeStatement st ModeExecDefault
    info  <- getQueryInfo  st 1
    fetch st
    value <- getQueryValue st 1
    v <- fromDataField DataField{..}
    return $ fromJust v

setupLanguage :: OracleConfig -> IO ()
setupLanguage conf = do
  nl <- lookupEnv "NLS_LANG"
  when (isNothing nl) $ getLanguage conf >>= setEnv "NLS_LANG" . unpack

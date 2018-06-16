{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Test.Hspec
import           Test.QuickCheck

import           System.Environment
import Data.Text(Text)
import qualified Data.Text.IO as TI

import           Database.Dpi
import           Database.Dpi.Field
import           Database.Dpi.Sql

conf :: OracleConfig
conf = OracleConfig "username" "password" "localhost:1521/dbname"

main :: IO ()
main = setupLanguage conf >> test

test :: IO ()
test = hspec $ do
  describe "Database.Dpi" $ do

    it "Context Test" $ do
      getEnv "NLS_LANG" >>= print
      c       <- createContext
      version <- getClientVersion c
      ok      <- destroyContext   c
      print version
      ok `shouldBe` True

    it "Connection Test" $ withContext $ \cxt -> do
      conn  <- createConnection cxt conf id id
      pOk   <- pingConnection conn
      pOk `shouldBe` True
      getEncodingInfo  conn >>= print
      getStmtCacheSize conn >>= print
      getServerVersion conn >>= print
      getLTXID         conn >>= print
      getInternalName  conn >>= print
      getExternalName  conn >>= print
      getEdition       conn >>= print
      getCurrentSchema conn >>= print
      ok    <- releaseConnection conn
      ok `shouldBe` True
      notOk <- releaseConnection conn
      notOk `shouldBe` False
      pNOk  <- pingConnection conn
      pNOk `shouldBe` False

    it "Statement 1 Test" $ withContext $ \cxt -> do
      withConnection cxt conf id id $ \conn -> do
        st <- prepareStatement conn False "SELECT '中文' FROM DUAL"
        c  <- getBindCount st
        c  `shouldBe` 0
        n  <- getBindNames st
        n  `shouldBe` []
        getFetchArraySize     st >>= print
        getImplicitResult     st >>= print
        getStatementInfo      st >>= print

        r  <- executeStatement st ModeExecDefault
        r `shouldBe` 1
        qc <- getNumberQueryColumns st
        qc `shouldBe` r

        getImplicitResult     st >>= print
        info <- getQueryInfo  st 1

        mayC <- fetch st
        mayC `shouldBe` Just 0

        value <- getQueryValue st 1
        Just k :: Maybe Text <- fromDataField DataField{..}
        print info
        TI.putStrLn k

        ok <- releaseStatement st
        ok `shouldBe` True

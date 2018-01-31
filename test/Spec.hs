{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Test.Hspec
import           Test.QuickCheck

import           Database.Dpi

-- define this 3 parameters before run test
username = "username"
password = "password"
connstr  = "localhost:1521/dbname"

main :: IO ()
main = hspec $ do
  describe "Database.Dpi" $ do

    it "Context Test" $ do
      c       <- createContext
      version <- getClientVersion c
      ok      <- destroyContext   c
      print version
      ok `shouldBe` True


    it "Connection Test" $ withContext $ \cxt -> do
      conn  <- createConnection cxt username password connstr id id
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
      withConnection cxt username password connstr "utf-8" "utf-8" $ \conn -> do
        st <- createStatement conn False "SELECT SYSDATE FROM DUAL"
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
        getQueryInfo  st 1       >>= print

        mayC <- fetch st
        mayC `shouldBe` Just 0

        getQueryValue st 1       >>= print

        ok <- releaseStatement st
        ok `shouldBe` True

    it "Statement 2 Test" $ withContext $ \cxt -> do
      withConnection cxt username password connstr "utf-8" "utf-8" $ \conn -> do
        withStatement conn False "SELECT 1,'中文' as 文字,SYSDATE FROM DUAL" $ \st -> do
          r <- executeStatement st ModeExecDefault
          r `shouldBe` 3
          f <- fetch st
          f `shouldBe` Just 0
          mapM (getQueryValue st) [1..r] >>= print
        withStatement conn False "SELECT 1,'hhh',SYSDATE FROM DUAL" $ \st -> do
          r <- executeStatement st ModeExecDefault
          r `shouldBe` 3
          f <- fetch st
          f `shouldBe` Just 0
          mapM (getQueryValue st) [1..r] >>= print
          f2 <- fetch st
          f2 `shouldBe` Nothing

    it "Statement 3 Failed Test" $ withContext $ \cxt -> do
      withConnection cxt username password connstr "" "" $ \conn -> do
        withStatement conn False "Wrong sql" $ \st -> do
          executeStatement st ModeExecDefault `shouldThrow` anyException

    it "Pool Test" $ withContext $ \cxt -> do
      withPool cxt username password connstr "utf-8" "utf-8" 2 $ \pool ->
        withPoolConnection pool $ \conn -> do
          let f _ = withStatement conn False "SELECT 1,'中文' as 文字,SYSDATE FROM DUAL" $ \st -> do
                      r <- executeStatement st ModeExecDefault
                      f <- fetch st
                      mapM (getQueryValue st) [1..r] >>= print
          mapM_ f [1..100]


printErr :: DpiException -> IO a
printErr e = print e >> throw e

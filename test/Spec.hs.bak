{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Test.Hspec
import           Test.QuickCheck

import           Database.Dpi
import           Database.Oracle

import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Data.Acquire         (with)
import           Data.Conduit
import qualified Data.Conduit.List    as CL
import           Data.Monoid          ((<>))
import qualified Data.Text            as T

-- define this 3 parameters before run test
username = "username"
password = "password"
connstr  = "localhost:1521/dbname"

prepareTable :: PtrConn -> IO ()
prepareTable conn = do
  execute conn "DROP TABLE TEST_T_NAME" []
  execute conn "CREATE TABLE TEST_T_NAME(ID INTEGER PRIMARY KEY, NAME VARCHAR2(64) NOT NULL, CREATE_TIME DATE DEFAULT SYSDATE NOT NULL)" []
  execute conn "DROP SEQUENCE TEST_SEQ_NAME" []
  execute conn "CREATE SEQUENCE TEST_SEQ_NAME" []
  return ()


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
        st <- prepareStatement conn False "SELECT SYSDATE FROM DUAL"
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
          mapM_ f [1..2]
          v <- queryByPage conn "SELECT DBTIMEZONE,CURRENT_DATE,CURRENT_TIMESTAMP,SYSDATE,SYSTIMESTAMP FROM dual" [] (0,1)
          print (v :: [DataRow])

          prepareTable conn
          let insert = "INSERT INTO TEST_T_NAME(ID,NAME) VALUES(:id,:name)"
          execute conn insert [("id",Nothing,False,DataInt 0),("name",Nothing,False,DataVarchar "test")]
          v <- queryByPage conn "SELECT * FROM TEST_T_NAME" [] (0,1)
          print (v::[DataRow])

          mapM_ (\i -> execute conn insert [("id",Nothing,False,DataInt i),("name",Nothing,False,DataVarchar ("test-" <> T.pack (show i)))]) [1..100]

          v <- queryByPage conn "SELECT * FROM TEST_T_NAME" [] (1,10)
          print (v::[DataRow])






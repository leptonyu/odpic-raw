{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-|

Module:      Database.Dpi
Copyright:   (c) Daniel YU
License:     BSD3
Maintainer:  leptonyu@gmail.com
Stability:   experimental
Portability: portable

FFI raw bindings to <https://oracle.github.io/odpi/doc ODPI-C>

@
import Database.Dpi

username = "username"
password = "password"
connstr  = "localhost:1521/dbname"

main :: IO ()
main = do
  withContext $ \\cxt ->
    withPool cxt username password connstr "utf-8" "utf-8" 2 $ \\pool ->
      withPoolConnection pool $ \\conn ->
        withStatement conn False "SELECT SYSDATE FROM DUAL" $ \\st -> do
          r <- executeStatement st ModeExecDefault
          f <- fetch st
          mapM (getQueryValue st) [1..r] >>= print
@

-}
module Database.Dpi
  ( module Database.Dpi
  , DpiException(..)
  , AuthMode(..)
  , ConnCloseMode(..)
  , CreateMode(..)
  , DeqMode(..)
  , DeqNavigation(..)
  , EventType(..)
  , ExecMode(..)
  , FetchMode(..)
  , MessageDeliveryMode(..)
  , MessageState(..)
  , NativeTypeNum(..)
  , OpCode(..)
  , OracleTypeNum(..)
  , PoolCloseMode(..)
  , PoolGetMode(..)
  , Purity(..)
  , ShutdownMode(..)
  , StartupMode(..)
  , StatementType(..)
  , SubscrNamespace(..)
  , SubscrProtocol(..)
  , SubscrQOS(..)
  , Visibility(..)
  , PtrConn
  , PtrPool
  , PtrStmt
  , PtrVar
  , PtrLob
  , PtrObject
  , PtrObjectAttr
  , PtrObjectType
  , PtrRowid
  , PtrSubscr
  , PtrDeqOptions
  , PtrEnqOptions
  , PtrMsgProps
  , PtrContext
  , Data_AppContext(..)
  , Data_CommonCreateParams(..)
  , Data_ConnCreateParams(..)
  , Data(..)
  , DataValue(..)
  , Data_Bytes(..)
  , Data_Timestamp(..)
  , Data_IntervalDS(..)
  , Data_IntervalYM(..)
  , Data_DataTypeInfo(..)
  , Data_EncodingInfo(..)
  , Data_ErrorInfo(..)
  , Data_ObjectAttrInfo(..)
  , Data_ObjectTypeInfo(..)
  , Data_PoolCreateParams(..)
  , Data_QueryInfo(..)
  , Data_ShardingKeyColumn(..)
  , Data_StmtInfo(..)
  , Data_SubscrCreateParams(..)
  , Data_SubscrMessage(..)
  , Data_SubscrMessageQuery(..)
  , Data_SubscrMessageRow(..)
  , Data_SubscrMessageTable(..)
  , Data_VersionInfo(..)
  , PtrAppContext
  , PtrCommonCreateParams
  , PtrConnCreateParams
  , PtrData
  , PtrDataTypeInfo
  , PtrEncodingInfo
  , PtrErrorInfo
  , PtrObjectAttrInfo
  , PtrObjectTypeInfo
  , PtrPoolCreateParams
  , PtrQueryInfo
  , PtrShardingKeyColumn
  , PtrStmtInfo
  , PtrSubscrCreateParams
  , PtrSubscrMessage
  , PtrSubscrMessageQuery
  , PtrSubscrMessageRow
  , PtrSubscrMessageTable
  , PtrVersionInfo
  , getContextError
  ) where

import           Database.Dpi.Internal
import           Database.Dpi.Prelude
import           Database.Dpi.Util

import           Control.Exception
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T

-- * Context Interface

-- $context
-- Context handles are the top level handles created by the library and are used for all
-- error handling as well as creating pools and standalone connections to the database.
-- The first call to ODPI-C by any application must be 'createContext' which will create the context
--  as well as validate the version used by the application. Context handles are destroyed by
-- using the function 'destroyContext'.

-- | Creates a new context for interaction with the library.
-- This is the first function that must be called and it must have completed successfully
-- before any other functions can be called, including in other threads.
createContext :: IO PtrContext
createContext = libContextCreate & inInt majorVersion & inInt minorVersion & go
  where
    go :: (Ptr PtrContext -> PtrErrorInfo -> IO CInt) -> IO PtrContext
    go f = withPtrs $ \(pc,pe) -> do
      i <- f pc pe
      if isOk i then peek pc else peek pe >>= throw . ErrorInfoException

-- | Destroys the context that was earlier created with the function 'createContext'.
destroyContext :: PtrContext -> IO Bool
destroyContext p = runBool libContextDestroy (p, p)

-- | With Context, 'PtrContext' will be destroyed after run
withContext :: (PtrContext -> IO a) -> IO a
withContext = bracket createContext destroyContext

-- ** Information from Context

-- | Return information about the version of the Oracle Client that is being used.
getClientVersion :: PtrContext -> IO Data_VersionInfo
getClientVersion p = libContextGetClientVersion p & outValue p peek


-- * Connection Interface
-- $connection
-- Connection handles are used to represent connections to the database.
-- These can be standalone connections created by calling the function 'createConnection'.
-- They can be closed by calling the function 'closeConnection' or releasing the last reference
-- to the connection by calling the function 'releaseConnection'.
-- Connection handles are used to create all handles other than session pools and context handles.


-- | Creates a standalone connection to a database or acquires a connection from a session pool and returns a reference to the connection.
createConnection :: PtrContext -- ^ Context
                 -> Text -- ^ the name of the user used for authenticating the user
                 -> Text -- ^ the password to use for authenticating the user
                 -> Text -- ^ the connect string identifying the database to which a connection is to be established
                 -> (Data_CommonCreateParams -> Data_CommonCreateParams) -- ^ custom 'Data_CommonCreateParams'
                 -> (Data_ConnCreateParams   -> Data_ConnCreateParams) -- ^ custom 'Data_ConnCreateParams'
                 -> IO PtrConn -- ^ a reference to the connection that is created.  
                 -- If a value is returned, a call to 'releaseConnection' must be made in order to release the reference. 
                 -- This should be done after the error information has been retrieved.
createConnection cxt username password connstr hcmp hccp
  = libConnCreate cxt
    & inStrLen username
    & inStrLen password
    & inStrLen connstr
    & inPtr (\c -> libContextInitCommonCreateParams cxt c >> peek c >>= poke c . hcmp)
    & inPtr (\c -> libContextInitConnCreateParams   cxt c >> peek c >>= poke c . hccp)
    & outValue cxt (peekWithCxt cxt)

-- | Closes the connection and makes it unusable for further activity. close connection, but not release resource, plese use 'releaseConnection' to release and close connection
closeConnection :: ConnCloseMode -> PtrConn -> IO Bool
closeConnection mode p = isOk <$> libConnClose (snd p) (fe mode) nullPtr 0

-- | Releases a reference to the connection. A count of the references to the connection is maintained
-- and when this count reaches zero, the memory associated with the connection is freed and
-- the connection is closed or released back to the session pool if that has not already taken place
--  using the function 'closeConnection'.
releaseConnection :: PtrConn -> IO Bool
releaseConnection = runBool libConnRelease

-- | Commits the current active transaction.
commitConnection :: PtrConn -> IO Bool
commitConnection = runBool libConnCommit

-- | Rolls back the current active transaction.
rollbackConnection :: PtrConn -> IO Bool
rollbackConnection = runBool libConnRollback

-- | Pings the database to verify that the connection is still alive.
pingConnection :: PtrConn -> IO Bool
pingConnection = runBool libConnPing

-- | with connection
withConnection
  :: PtrContext -- ^ Context
  -> Text -- ^ the name of the user used for authenticating the user
  -> Text -- ^ the password to use for authenticating the user
  -> Text -- ^ the connect string identifying the database to which a connection is to be established
  -> Text -- ^ NLS_LANG encoding
  -> Text -- ^ NLS_NCHAR encoding
  -> (PtrConn -> IO a) -- ^ action use connection
  -> IO a
withConnection p username password connstr lang nchar
  = bracket
      (createConnection p username password connstr (set lang nchar) id)
      (\c -> closeConnection ModeConnCloseDefault c `finally` releaseConnection c)
  where
    set l n v = v { encoding = l, nencoding = n} :: Data_CommonCreateParams

-- ** Transaction Interface

-- | Begins a distributed transaction using the specified transaction id (XID) made up of the formatId, transactionId and branchId.
beginDistributedTransaction
  :: PtrConn -- ^ Connection
  -> Int64   -- ^  the identifier of the format of the XID. A value of -1 indicates that the entire XID is null.
  -> Text    -- ^  the global transaction id of the XID as a byte string. The maximum length permitted is 64 bytes.
  -> Text    -- ^ the branch id of the XID as a byte string. The maximum length permitted is 64 bytes.
  -> IO Bool
beginDistributedTransaction p formatId transId branchId
  = libConnBeginDistribTrans (snd p) (fromIntegral formatId)
    & inStrLen transId
    & inStrLen branchId
    & outBool

-- | Prepares a distributed transaction for commit.
-- This function should only be called after 'beginTransaction' is called and before 'commitConnection' is called.
prepareDistributedTransaction :: PtrConn -> IO Bool
prepareDistributedTransaction (cxt,p) = libConnPrepareDistribTrans p & outValue cxt peekBool

-- ** Information from Connection

-- | Returns the current schema that is being used by the connection.
getCurrentSchema :: PtrConn -> IO Text
getCurrentSchema = runText libConnGetCurrentSchema

-- | Sets the current schema to be used on the connection. 
-- This has the same effect as the SQL statement ALTER SESSION SET CURRENT_SCHEMA. 
-- The value be changed when the next call requiring a round trip to the server is performed. 
-- If the new schema name does not exist, the same error is returned as when the alter session statement is executed. 
-- The new schema name is placed before database objects in statement that you execute that do not already have a schema.
setCurrentSchema :: PtrConn -> Text -> IO Bool
setCurrentSchema = setText libConnSetCurrentSchema

-- | Returns the edition that is being used by the connection.
getEdition :: PtrConn -> IO Text
getEdition = runText libConnGetEdition

-- | Returns the external name that is being used by the connection. This value is used when logging distributed transactions.
getExternalName :: PtrConn -> IO Text
getExternalName = runText libConnGetExternalName

-- | Sets the external name that is being used by the connection. This value is used when logging distributed transactions.
setExternalName :: PtrConn -> Text -> IO Bool
setExternalName = setText libConnSetExternalName

-- | Returns the internal name that is being used by the connection. This value is used when logging distributed transactions.
getInternalName :: PtrConn -> IO Text
getInternalName = runText libConnGetInternalName

-- | Sets the internal name that is being used by the connection. This value is used when logging distributed transactions.
setInternalName :: PtrConn -> Text -> IO Bool
setInternalName = setText libConnSetInternalName

-- | Returns the logical transaction id for the connection. 
-- This value is used in Transaction Guard to determine if the last failed call was completed 
  -- and if the transaction was committed using the procedure call dbms_app_cont.get_ltxid_outcome().
getLTXID :: PtrConn -> IO Text
getLTXID = runText libConnGetLTXID

-- | Returns the version information of the Oracle Database to which the connection has been made.
getServerVersion :: PtrConn -> IO (Text, Data_VersionInfo)
getServerVersion (cxt,p) = libConnGetServerVersion p & out3Value cxt go
  where
    go (pl,pv) = (,) <$> peekCStrLen pl <*> peek pv

-- | Looks up an object type by name in the database and returns a reference to it. 
-- The reference should be released as soon as it is no longer needed.
getObjectType :: PtrConn -> Text -> IO PtrObjectType
getObjectType (cxt,p) name
  = libConnGetObjectType p
    & inStrLen name
    & outValue cxt (peekWithCxt cxt)

-- | Returns the encoding information used by the connection. 
-- This will be equivalent to the values passed when the standalone connection or session pool was created, 
-- or the values retrieved from the environment variables NLS_LANG and NLS_NCHAR.
getEncodingInfo :: PtrConn -> IO Data_EncodingInfo
getEncodingInfo = runVar libConnGetEncodingInfo

-- | Returns the size of the statement cache, in number of statements.
getStmtCacheSize :: PtrConn -> IO Int
getStmtCacheSize = runInt libConnGetStmtCacheSize

-- | Sets the size of the statement cache.
setStmtCacheSize :: PtrConn -> Int -> IO Bool
setStmtCacheSize (cxt,p) size = libConnSetStmtCacheSize p & inInt size & outBool

-- | Sets the client info attribute on the connection. 
-- This is one of the end-to-end tracing attributes that can be tracked in database views, 
-- shown in audit trails and seen in tools such as Enterprise Manager.
setClientInfo :: PtrConn -> Text -> IO Bool
setClientInfo = setText libConnSetClientInfo

-- | Sets the client identifier attribute on the connection. 
-- This is one of the end-to-end tracing attributes that can be tracked in database views, 
-- shown in audit trails and seen in tools such as Enterprise Manager.
setClientIdentifier :: PtrConn -> Text -> IO Bool
setClientIdentifier = setText libConnSetClientIdentifier

-- | Sets the action attribute on the connection. This is one of the end-to-end tracing attributes 
-- that can be tracked in database views, shown in audit trails and seen in tools such as Enterprise Manager.
setAction :: PtrConn -> Text -> IO Bool
setAction = setText libConnSetAction

-- | Sets the database operation attribute on the connection. 
-- This is one of the end-to-end tracing attributes that can be tracked in database views, 
-- shown in audit trails and seen in tools such as Enterprise Manager.
setDbOp :: PtrConn -> Text -> IO Bool
setDbOp = setText libConnSetDbOp

-- | Sets the module attribute on the connection. 
-- This is one of the end-to-end tracing attributes that can be tracked in database views, 
-- shown in audit trails and seen in tools such as Enterprise Manager.
setModule :: PtrConn -> Text -> IO Bool
setModule = setText libConnSetModule

-- | Returns the OCI service context handle in use by the connection.
getHandler :: PtrConn -> IO (Ptr ())
getHandler = runVar libConnGetHandle

-- ** Connection Management

-- | Adds a reference to the connection. 
-- This is intended for situations where a reference to the connection needs to be maintained independently of 
  -- the reference returned when the connection was created.
connectionAddRef :: PtrConn -> IO Bool
connectionAddRef = runBool libConnAddRef

-- | Performs an immediate (asynchronous) termination of any currently executing function on the server associated with the connection.
breakException :: PtrConn -> IO Bool
breakException = runBool libConnBreakExecution

-- | Changes the password of the specified user.
changePassword
  :: PtrConn -- ^ Connection
  -> Text    -- ^ the name of the user whose password is to be changed
  -> Text    -- ^ the old password of the user whose password is to be changed
  -> Text    -- ^ the new password of the user whose password is to be changed
  -> IO Bool
changePassword (cxt,p) username oldPassword newPassword
  = libConnChangePassword p
    & inStrLen username
    & inStrLen oldPassword
    & inStrLen newPassword
    & outBool

-- | Shuts down the database. This function must be called twice for the database to be shut down successfully. 
-- After calling this function the first time, the SQL statements “alter database close normal” 
-- and “alter database dismount” must be executed. 
-- Once that is complete this function should be called again with the mode 'ModeShutdownFinal' 
-- in order to complete the orderly shutdown of the database.
shutdownDatabase 
  :: PtrConn      -- ^ a reference to the connection to the database which is to be shut down.
  -- The connection needs to have been established at least with authorization mode set 
  -- to 'ModeAuthSysdba' or 'ModeAuthSysoper'.
  -> ShutdownMode 
  -> IO Bool
shutdownDatabase (cxt,p) sm = libConnShutdownDatabase p & inEnum sm & outBool

-- | Starts up a database.
startupDatabase 
  :: PtrConn     -- ^ a reference to the connection to the database which is to be started up.
  -- A connection like this can only be created with the authorization mode set to 'ModeAuthPrelim' along with 
  -- one of 'ModeAuthSysdba' or 'ModeAuthSysoper'.
  -> StartupMode -- ^ one of the values from the enumeration 'StartupMode'
  -> IO Bool
startupDatabase (cxt,p) sm = libConnStartupDatabase p & inEnum sm & outBool


-- * ConnectionPool Interace
-- $pool
-- Pool handles are used to represent session pools.
-- They are created using the function 'createPool' and can be closed by calling the function 'closePool'
-- or releasing the last reference to the pool by calling the function 'releasePool'.
-- Pools can be used to create connections by calling the function 'acquiredConnection'.

-- | Acquires a connection from the pool and returns a reference to it. This reference should be released as soon as it is no longer needed.
acquiredConnection :: PtrPool -> IO PtrConn
acquiredConnection (cxt,p) = libPoolAcquireConnection p nullPtr 0 nullPtr 0 nullPtr & outValue cxt (peekWithCxt cxt)

-- | Adds a reference to the pool. This is intended for situations where a reference to 
-- the pool needs to be maintained independently of the reference returned when the pool was created.
poolAddRef :: PtrPool -> IO Bool
poolAddRef = runBool libPoolAddRef

-- | Creates a session pool which creates and maintains a group of stateless sessions to the database.
--  The main benefit of session pooling is performance since making a connection to the database is a time-consuming activity,
-- especially when the database is remote.
createPool
  :: PtrContext -- ^ Context
  -> Text -- ^ the name of the user used for authenticating the user
  -> Text -- ^ the password to use for authenticating the user
  -> Text -- ^ the connect string identifying the database to which a connection is to be established
  -> (Data_CommonCreateParams -> Data_CommonCreateParams) -- ^ custom 'Data_CommonCreateParams'
  -> (Data_PoolCreateParams -> Data_PoolCreateParams)
  -> IO PtrPool
createPool cxt username password connstr hcmp hpcp
  = libPoolCreate cxt
    & inStrLen username
    & inStrLen password
    & inStrLen connstr
    & inPtr (\c -> libContextInitCommonCreateParams cxt c >> peek c >>= poke c . hcmp)
    & inPtr (\c -> libContextInitPoolCreateParams   cxt c >> peek c >>= poke c . hpcp)
    & outValue cxt (peekWithCxt cxt)

-- | Closes the pool and makes it unusable for further activity.
closePool :: PtrPool -> PoolCloseMode -> IO Bool
closePool (cxt,p) mode = libPoolClose p & inEnum mode & outBool

-- | Releases a reference to the pool. A count of the references to the pool is maintained
-- and when this count reaches zero, the memory associated with the pool is freed
-- and the session pool is closed if that has not already taken place using the function 'closePool'.
releasePool :: PtrPool -> IO Bool
releasePool = runBool libPoolRelease

-- | with pool
withPool
  :: PtrContext -- ^ Context
  -> Text -- ^ the name of the user used for authenticating the user
  -> Text -- ^ the password to use for authenticating the user
  -> Text -- ^ the connect string identifying the database to which a connection is to be established
  -> Text -- ^ NLS_LANG encoding
  -> Text -- ^ NLS_NCHAR encoding
  -> Int
  -> (PtrPool -> IO a) -- ^ action use connection
  -> IO a
withPool p username password connstr lang nchar thread
  = bracket
      (createPool p username password connstr (set lang nchar) (setP thread))
      (\c -> closePool c ModePoolCloseDefault `finally` releasePool c)
  where
    set l n v = v { encoding  = if T.null l then  encoding (v :: Data_CommonCreateParams) else l
                  , nencoding = if T.null n then nencoding (v :: Data_CommonCreateParams) else n} :: Data_CommonCreateParams
    setP t  v = v { maxSessions = fromIntegral t, sessionIncrement = 1 } :: Data_PoolCreateParams

-- | with pool provide a connection will released after action
withPoolConnection :: PtrPool -> (PtrConn -> IO a) -> IO a
withPoolConnection p = bracket (acquiredConnection p) releaseConnection

-- | Returns the number of sessions in the pool that are busy.
getPoolBusyCount :: PtrPool -> IO Int
getPoolBusyCount = runInt libPoolGetBusyCount

-- | Returns the encoding information used by the pool. 
-- This will be equivalent to the values passed when the pool was created, 
-- or the values retrieved from the environment variables NLS_LANG and NLS_NCHAR.
getPoolEncodingInfo :: PtrPool -> IO Data_EncodingInfo
getPoolEncodingInfo = runVar libPoolGetEncodingInfo

-- | Returns the mode used for acquiring or getting connections from the pool.
getPoolMode :: PtrPool -> IO PoolGetMode
getPoolMode (cxt,p) = libPoolGetGetMode p & outValue cxt peekEnum

-- | Returns the maximum lifetime of all sessions in the pool, in seconds. 
-- Sessions in the pool are terminated when this value has been reached, 
-- but only when another session is released back to the pool.
getPoolMaxLifetimeSession :: PtrPool -> IO Int
getPoolMaxLifetimeSession = runInt libPoolGetMaxLifetimeSession

-- | Returns the number of sessions in the pool that are open.
getPoolOpenCount :: PtrPool -> IO Int
getPoolOpenCount = runInt libPoolGetOpenCount

-- | Returns the default size of the statement cache for sessions in the pool, in number of statements.
getPoolStmtCacheSize :: PtrPool -> IO Int
getPoolStmtCacheSize = runInt libPoolGetStmtCacheSize

-- | Returns the amount of time, in seconds, after which idle sessions in the pool are terminated, 
-- but only when another session is released back to the pool.
getPoolTimeout :: PtrPool -> IO Int
getPoolTimeout = runInt libPoolGetTimeout

-- | Sets the mode used for acquiring or getting connections from the pool.
setPoolGetMode :: PtrPool -> PoolGetMode -> IO Bool
setPoolGetMode (cxt,p) mode = libPoolSetGetMode p & inEnum mode & outBool

-- | Sets the maximum lifetime of all sessions in the pool, in seconds. 
-- Sessions in the pool are terminated when this value has been reached, 
-- but only when another session is released back to the pool.
setPoolMaxLifetimeSession :: PtrPool -> Int -> IO Bool
setPoolMaxLifetimeSession (cxt,p) maxLifetimeSession
  = libPoolSetMaxLifetimeSession p
    & inInt maxLifetimeSession
    & outBool

-- | Sets the default size of the statement cache for sessions in the pool.
setPoolStmtCacheSize :: PtrPool -> Int -> IO Bool
setPoolStmtCacheSize (cxt,p) stmtCacheSize
  = libPoolSetStmtCacheSize p
    & inInt stmtCacheSize
    & outBool

-- | Sets the amount of time, in seconds, after which idle sessions in the pool are terminated, 
-- but only when another session is released back to the pool.
setPoolTimeout :: PtrPool -> Int -> IO Bool
setPoolTimeout (cxt,p) timeout
  = libPoolSetTimeout p
    & inInt timeout
    & outBool

-- * Statement Interface
-- $statement
-- Statement handles are used to represent statements of all types (queries, DML, DDL and PL/SQL).
--  They are created by calling the function 'prepareStatement'.
-- They are also created implicitly when a variable of type 'OracleTypeStmt' is created.
--  Statement handles can be closed by calling the function 'closeStatement'
-- or by releasing the last reference to the statement by calling the function 'releaseStatement'.

-- | Returns a reference to a statement prepared for execution.
-- The reference should be released as soon as it is no longer needed.
prepareStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ a boolean indicating if the statement is scrollable or not. 
    -- If it is scrollable, 'scrollStatement' can be used to reposition the cursor; 
    -- otherwise, rows are retrieved in order from the statement until the rows are exhausted. 
    -- This value is ignored for statements that do not refer to a query.
  -> Text       -- ^ SQL String, not allow to use multi lines or semicolon as end of sql.
                -- use 'normalize' use normalize sql text.
  -> IO PtrStmt
prepareStatement (cxt,p) scrollable sql
  = libConnPrepareStmt p
    & inBool scrollable
    & inStrLen sql
    & (\f -> f nullPtr 0)
    & outValue cxt (peekWithCxt cxt)

-- | Normalize SQL, replace newline characters with space characters. and remove semicolon in the end of sql
normalize :: Text -> Text
normalize = T.dropWhileEnd (==';')
          . T.strip
          . T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

-- | Closes the statement and makes it unusable for further work immediately,
-- rather than when the reference count reaches zero.
closeStatement :: PtrStmt -> IO Bool
closeStatement (cxt, p) = isOk <$> libStmtClose p nullPtr 0

-- | Releases a reference to the statement. A count of the references to the statement is maintained
-- and when this count reaches zero, the memory associated with the statement is freed
-- and the statement is closed if that has not already taken place using the function 'closeStatement'.
releaseStatement :: PtrStmt -> IO Bool
releaseStatement = runBool libStmtRelease

-- | with statement provide a prepared statement will released after action
withStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ a boolean indicating if the statement is scrollable or not. 
    -- If it is scrollable, 'scrollStatement' can be used to reposition the cursor; 
    -- otherwise, rows are retrieved in order from the statement until the rows are exhausted. 
    -- This value is ignored for statements that do not refer to a query.
  -> Text       -- ^ SQL String, not allow to use multi lines or semicolon as end of sql.
                -- use 'normalize' use normalize sql text.
  -> (PtrStmt -> IO a)
  -> IO a
withStatement p scrollable sql f
  = bracket
      (prepareStatement p scrollable sql)
      releaseStatement
      $ \s -> do a <- f s
                 a `seq` return a

-- | Scrolls the statement to the position in the cursor specified by the mode and offset.
scrollStatement :: PtrStmt -> FetchMode -> Int -> Int -> IO Bool
scrollStatement (cxt,p) mode offset rowOffset = libStmtScroll p & inEnum mode & inInt offset & inInt rowOffset & outBool

-- | Adds a reference to the statement. 
-- This is intended for situations where a reference to the statement needs to be maintained independently of 
  -- the reference returned when the statement was created.
statementAddRef :: PtrStmt -> IO Bool
statementAddRef = runBool libStmtAddRef

-- ** Statement Bind Vars

-- | Binds a variable to a named placeholder in the statement.
-- A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same name.
bindByName 
  :: PtrStmt -- ^ Statement
  -> Text    -- ^ a byte string in the encoding used for CHAR data giving the name of the placeholder which is to be bound.
  -> PtrVar  -- ^ a reference to the variable which is to be bound. 
  -> IO Bool
bindByName (cxt,p) name (_,var)
  = libStmtBindByName p
    & inStrLen name
    & inVar var
    & outBool

-- | Binds a variable to a placeholder in the statement by position.
--  A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same position.
bindByPosition 
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position which is to be bound. 
             -- The position of a placeholder is determined by its location in the statement. 
             -- Placeholders are numbered from left to right, starting from 1, and duplicate names do not count as additional placeholders.
  -> PtrVar  -- ^ a reference to the variable which is to be bound. 
  -> IO Bool
bindByPosition (cxt,p) pos (_,var)
  = libStmtBindByPos p
    & inInt pos
    & inVar var
    & outBool

-- | Binds a value to a named placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same name.
bindValueByName 
  :: PtrStmt       -- ^ Statement
  -> Text          -- ^ a byte string in the encoding used for CHAR data giving the name of the placeholder which is to be bound.
  -> NativeTypeNum -- ^ the type of data that is being bound.
  -> PtrData       -- ^ the data which is to be bound, as a pointer to a dpiData structure. 
                   -- A variable will be created based on the type of data being bound 
                   -- and a reference to this variable retained. 
                   -- Once the statement has been executed, this new variable will be released.
  -> IO Bool
bindValueByName (cxt,p) name ntn dt
  = libStmtBindValueByName p
    & inStrLen name
    & inEnum ntn
    & inVar dt
    & outBool

-- | Binds a value to a placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same position.
bindValueByPosition 
  :: PtrStmt       -- ^ Statement
  -> Int           -- ^  the position which is to be bound. 
                   -- The position of a placeholder is determined by its location in the statement. 
                   -- Placeholders are numbered from left to right, starting from 1, 
                   -- and duplicate names do not count as additional placeholders.
  -> NativeTypeNum -- ^ the type of data that is being bound.
  -> PtrData       -- ^ the data which is to be bound, as a pointer to a dpiData structure. 
                   -- A variable will be created based on the type of data being bound 
                   -- and a reference to this variable retained. 
                   -- Once the statement has been executed, this new variable will be released.
  -> IO Bool
bindValueByPosition (cxt,p) pos ntn dt
  = libStmtBindValueByPos p
    & inInt pos
    & inEnum ntn
    & inVar dt
    & outBool

-- | Defines the variable that will be used to fetch rows from the statement.
-- A reference to the variable will be retained until the next define is performed on the same position
-- or the statement is closed.
define 
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position which is to be defined. The first position is 1.
  -> PtrVar  -- ^ a reference to the variable which is to be used for fetching rows from the statement at the given position. 
  -> IO Bool
define (cxt,p) pos (_,var)
  = libStmtDefine p
    & inInt pos
    & inVar var
    & outBool

-- | Defines the type of data that will be used to fetch rows from the statement.
-- This is intended for use with the function 'getQueryValue', when the default data type
-- derived from the column metadata needs to be overridden by the application.
-- Internally, a variable is created with the specified data type and size.
defineValue 
  :: PtrStmt       -- ^ Statement
  -> Int           -- ^ the position which is to be defined. The first position is 1.
  -> OracleTypeNum -- ^ the type of Oracle data that is to be used.
  -> NativeTypeNum -- ^ the type of native C data that is to be used. 
  -> Int           -- ^ the maximum size of the buffer used for transferring data to/from Oracle. 
                   -- This value is only used for variables transferred as byte strings. 
                   -- Size is either in characters or bytes depending on the value of the sizeIsBytes parameter. 
                   -- If the value is in characters, internally the value will be multipled by the maximum number of bytes 
                   -- for each character and that value used instead when determining the necessary buffer size.
  -> Bool          -- ^ boolean value indicating if the size parameter refers to characters or bytes. 
                   -- This flag is only used if the variable refers to character data.
  -> PtrObjectType -- ^ a reference to the object type of the object that is being bound or fetched. 
                   -- This value is only used if the Oracle type is 'OracleTypeObject'.
  -> IO Bool
defineValue (cxt,p) pos otn ntn size isSizeInByte (_,ot)
  = libStmtDefineValue p
    & inInt pos
    & inEnum otn
    & inEnum ntn
    & inInt size
    & inBool isSizeInByte
    & inVar ot
    & outBool

-- | Returns the number of bind variables in the prepared statement.
-- In SQL statements this is the total number of bind variables whereas in PL/SQL statements
-- this is the count of the unique bind variables.
getBindCount :: PtrStmt -> IO Int
getBindCount = runInt libStmtGetBindCount

-- | Returns the names of the unique bind variables in the prepared statement.
getBindNames :: PtrStmt -> IO [Text]
getBindNames ps@(cxt,p) = do
  c <- getBindCount ps
  alloca $ \pn  ->
    allocaArray c $ \pan  ->
    alloca $ \panl -> do
      ok <- isOk <$> libStmtGetBindNames p pn pan panl
      if ok
        then do
          n  <- peek pn
          ac <- peekArray (fromIntegral n) pan
          al <- peek panl
          mapM (`ts` al) ac
        else getContextError cxt >>= throw . ErrorInfoException

-- | Returns information about the statement.
getStatementInfo :: PtrStmt -> IO Data_StmtInfo
getStatementInfo = runVar libStmtGetInfo

-- | Gets the array size used for performing fetches.
getFetchArraySize :: PtrStmt -> IO Int
getFetchArraySize = runInt libStmtGetFetchArraySize

-- | Sets the array size used for performing fetches. 
-- All variables defined for fetching must have this many (or more) elements allocated for them. 
-- The higher this value is the less network round trips are required to fetch rows from the database 
-- but more memory is also required. A value of zero will reset the array size to 
-- the default value of DPI_DEFAULT_FETCH_ARRAY_SIZE.
setFetchArraySize :: PtrStmt -> Int -> IO Bool
setFetchArraySize (cxt,p) pos
  = libStmtSetFetchArraySize p
    & inInt pos
    & outBool

-- | Returns the next implicit result available from the last execution of the statement. 
-- Implicit results are only available when both the client and server are 12.1 or higher.
getImplicitResult :: PtrStmt -> IO (Maybe PtrStmt)
getImplicitResult p@(cxt,_) = do
  ps <- runMaybeVar libStmtGetImplicitResult p
  return $ fmap (cxt,) ps

-- | Returns the number of columns that are being queried.
getNumberQueryColumns :: PtrStmt -> IO Int
getNumberQueryColumns = runInt libStmtGetNumQueryColumns

-- | Returns information about the column that is being queried.
getQueryInfo
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO Data_QueryInfo
getQueryInfo (cxt,p) pos
  = libStmtGetQueryInfo p
    & inInt pos
    & outValue cxt peek

-- | Returns the value of the column at the given position for the currently fetched row,
-- without needing to provide a variable. If the data type of the column needs to be overridden,
-- the function 'defineValue' can be called to specify a different type after executing
-- the statement but before fetching any data.
getQueryValue
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO DataValue
getQueryValue (cxt,p) pos
  = libStmtGetQueryValue p
    & inInt pos
    & out2Value cxt go
    where
      go (pt,pd) = do
        t <- te <$> peek pt
        peek pd >>= _get t

-- ** Execute Statement

-- | Executes the statement using the bound values.
-- For queries this makes available metadata which can be acquired using the function 'getQueryInfo'.
-- For non-queries, out and in-out variables are populated with their values.
executeStatement :: PtrStmt -> ExecMode -> IO Int
executeStatement ps@(cxt,p) mode = libStmtExecute p & inEnum mode & outValue cxt peekInt

executeMany :: PtrStmt -> ExecMode -> Int -> IO Bool
executeMany (_,p) mode count
  = libStmtExecuteMany p
    & inEnum mode
    & inInt  count
    & outBool

-- | Fetches a single row from the statement. If the statement does not refer to a query an error is returned.
--  All columns that have not been defined prior to this call are implicitly defined using the metadata made available when the statement was executed.
fetch :: PtrStmt -> IO (Maybe PageOffset)
fetch (cxt,p)
  = libStmtFetch p
    & out2Value cxt go
    where
      go (pf,pr) = do
        found <- toBool <$> peek pf
        if found then Just <$> peekInt pr else return Nothing

-- Index, RowNum
type PageOffset = Int64
type PageLimit  = Int64
type Page = (PageOffset, PageLimit)
type FetchRows a = PtrStmt -> Page -> IO a

-- | Returns the number of rows that are available in the buffers defined for the query.
--  If no rows are currently available in the buffers, an internal fetch takes place in order to populate them,
-- if rows are available. If the statement does not refer to a query an error is returned.
-- All columns that have not been defined prior to this call are implicitly defined using
-- the metadata made available when the statement was executed.
fetchRows
  :: PtrStmt -- ^ Statement
  -> Int     -- ^  the maximum number of rows to fetch. If the number of rows available exceeds this value only this number will be fetched.
  -> IO (Bool, [DataValue])
fetchRows ps@(cxt,p) maxRow
  = libStmtFetchRows p
    & inInt maxRow
    & out3Value cxt (go ps)
    where
      go ps ((pri,prf), pmr) = do
        index <- peekInt pri
        num   <- peekInt prf
        vs    <- fetch ps index num
        more  <- toBool <$> peek pmr
        return (more, vs)
      fetch p offset limit = do
        count <- getRowCount p
        mapM (getQueryValue p) [1..count]

-- | Returns the number of rows affected by the last DML statement that was executed
-- or the number of rows currently fetched from a query. In all other cases 0 is returned.
getRowCount :: PtrStmt -> IO Int
getRowCount = runInt libStmtGetRowCount

-- | Returns an array of row counts affected by the last invocation of 'executeMany'
-- with the array DML rowcounts mode enabled.
-- This feature is only available if both client and server are at 12.1.
getRowCounts :: PtrStmt -> IO [Int]
getRowCounts (cxt,p)
  = libStmtGetRowCounts p
    & out2Value cxt go
    where
      go (pc, pac) = do
        c   <- peekInt pc
        pcs <- peekArray c pac
        mapM peekInt pcs

-- | Returns the id of the query that was just registered on the subscription by calling 'executeStatement'
-- on a statement prepared by calling 'prepareStatement'.
getSubscrQueryId :: PtrStmt -> IO Word64
getSubscrQueryId = runInt libStmtGetSubscrQueryId

getBatchErrorCount :: PtrStmt -> IO Int
getBatchErrorCount = runInt libStmtGetBatchErrorCount

getBatchErrors :: PtrStmt -> IO [Data_ErrorInfo]
getBatchErrors ps@(cxt,p) = do
  c <- getBatchErrorCount ps
  if c <= 0
    then return []
    else do
      allocaArray c $ \par -> do
        ok <- libStmtGetBatchErrors p & inInt c & inVar par & outBool
        if ok then peekArray c par else throwContextError cxt

-- * Lob Interface
-- $lob
-- LOB handles are used to represent large objects (CLOB, BLOB, NCLOB, BFILE). 
-- Both persistent and temporary large objects can be represented. 
-- LOB handles can be created by calling the function 'newTempLob' or are created implicitly 
-- when a variable of type 'OracleTypeClob', 'OracleTypeNclob', 'OracleTypeBlob' or 'OracleTypeBfile' 
-- is created and are destroyed when the last reference is released by calling the function 'releaseLob'. 
-- They are used for reading and writing data to the database in smaller pieces than is contained in the large object.

-- | Adds a reference to the LOB. 
-- This is intended for situations where a reference to the LOB needs to be maintained independently of 
-- the reference returned when the LOB was created.
lobAddRef :: PtrLob -> IO Bool
lobAddRef = runBool libLobAddRef

-- | Returns a reference to a new temporary LOB which may subsequently be written and bound to a statement. 
-- The reference should be released as soon as it is no longer needed.
newTempLob :: PtrConn -> OracleTypeNum -> IO PtrLob
newTempLob (cxt,p) otn
  = libConnNewTempLob p
    & inEnum otn
    & outValue cxt (peekWithCxt cxt)

-- | close lob
closeLob :: PtrLob -> IO Bool
closeLob = runBool libLobClose

-- | Releases a reference to the LOB. 
-- A count of the references to the LOB is maintained and when this count reaches zero, 
-- the memory associated with the LOB is freed. 
-- The LOB is also closed unless that has already taken place using the function 'closeLob'.
releaseLob :: PtrLob -> IO Bool
releaseLob = runBool libLobRelease

-- | Trims the data in the LOB so that it only contains the specified amount of data.
trimLob :: PtrLob -> Int64 -> IO Bool
trimLob (cxt,p) size = libLobTrim p & inInt size & outBool

-- | Closes the LOB resource. 
-- This should be done when a batch of writes has been completed so that the indexes associated with the LOB can be updated. 
-- It should only be performed if a call to function 'openLobResource' has been performed.
closeLobResource :: PtrLob -> IO Bool
closeLobResource = runBool libLobCloseResource

-- | Opens the LOB resource for writing. 
-- This will improve performance when writing to the LOB in chunks 
-- and there are functional or extensible indexes associated with the LOB. 
-- If this function is not called, the LOB resource will be opened and closed for each write that is performed. 
-- A call to the function 'closeLobResource' should be done before performing a call to the function 'commitConnection'.
openLobResource :: PtrLob -> IO Bool
openLobResource = runBool libLobOpenResource

-- | Returns a boolean value indicating if the LOB resource has been opened by making 
-- a call to the function 'openLobResource' (1) or not (0).
isLobResourceOpen :: PtrLob -> IO Bool
isLobResourceOpen (cxt,p) = libLobGetIsResourceOpen p & outValue cxt peekBool

-- | Creates an independent copy of a LOB and returns a reference to the newly created LOB.
-- This reference should be released as soon as it is no longer needed.
copyLob :: PtrLob -> IO PtrLob
copyLob p@(cxt,_)= (cxt,) <$> runVar libLobCopy p

-- | Flush or write all buffers for this LOB to the server.
flushLob :: PtrLob -> IO Bool
flushLob = runBool libLobFlushBuffer

-- | Returns the size of the buffer needed to hold the number of characters specified for a buffer of the type 
-- associated with the LOB. If the LOB does not refer to a character LOB the value is returned unchanged.
getLobBufferSize :: PtrLob -> Word64 -> IO Word64
getLobBufferSize (cxt,p) size
  = libLobGetBufferSize p
    & inInt size
    & outValue cxt peekInt

-- | Returns the chunk size of the internal LOB. Reading and writing to the LOB in multiples of this size will improve performance.
getLobChunkSize :: PtrLob -> IO Int64
getLobChunkSize = runInt libLobGetChunkSize

-- | Returns the directory alias name and file name for a BFILE type LOB.
getLobDirectoryAndFileName :: PtrLob -> IO (FilePath, String)
getLobDirectoryAndFileName (cxt,p) = libLobGetDirectoryAndFileName p & out4Value cxt go
  where
    go ((pd, pdlen), (pn, pnlen)) = do
      d    <- peek pd
      dlen <- peek pdlen
      n    <- peek pn
      nlen <- peek pnlen
      fp   <- ts d dlen
      name <- ts n nlen
      return (T.unpack fp, T.unpack name)

-- | Sets the directory alias name and file name for a BFILE type LOB.
setLobDirectoryAndFileName :: PtrLob -> (FilePath, String) -> IO Bool
setLobDirectoryAndFileName (cxt,p) (fp, name)
  = libLobSetDirectoryAndFileName p
    & inStrLen fp
    & inStrLen name
    & outBool

-- | Returns a boolean value indicating if the file referenced by the BFILE type LOB exists (1) or not (0).
lobFileExists :: PtrLob -> IO Bool
lobFileExists (cxt,p) = libLobGetFileExists p & outValue cxt peekBool

-- | Returns the size of the data stored in the LOB. 
-- For character LOBs the size is in characters; for binary LOBs the size is in bytes.
getLobSize :: PtrLob -> IO Int64
getLobSize = runInt libLobGetSize

-- | Replaces all of the data in the LOB with the contents of the provided buffer. 
-- The LOB will first be cleared and then the provided data will be written.
setLobFromBytes :: PtrLob -> Text -> IO Bool
setLobFromBytes (cxt,p) buff
  = libLobSetFromBytes p
    & inStrLen buff
    & outBool

type BufferSize = Int64

-- | Reads data from the LOB at the specified offset into the provided buffer.
readLobBytes :: PtrLob -> Page -> BufferSize -> IO Text
readLobBytes (cxt,p) (offset, num) bufferSize
  = libLobReadBytes p
    & inInt offset
    & inInt num
    & uncurry
    & outValue' cxt get (set bufferSize)
    where
      set bs (pb,pblen) = poke pblen (fromIntegral bs)
      get    (pb,pblen) = do
        pl <- peek pblen
        ts pb (fromIntegral pl)

-- | Write data to the LOB at the specified offset using the provided buffer as the source. 
-- If multiple calls to this function are planned, the LOB should first be opened using the function 'openLob'.
writeLobBytes :: PtrLob -> PageOffset -> Text -> IO Bool
writeLobBytes (cxt,p) size buff
  = libLobWriteBytes p
    & inInt size
    & inStrLen buff
    & outBool

-- * Object Interface
-- $object
-- Object handles are used to represent instances of the types created by the SQL command CREATE OR REPLACE TYPE. 
-- They are created by calling the function 'createObject' or calling the function 'copyObject' or implicitly by 
-- creating a variable of the type OracleTypeObject. 
-- The are destroyed when the last reference is released by calling the function 'releaseObject'.

-- | Creates an object of the specified type and returns a reference to it. 
-- This reference should be released as soon as it is no longer needed.
createObject :: PtrObjectType -> IO PtrObject
createObject p@(cxt,_)= (cxt,) <$> runVar libObjectTypeCreateObject p

-- | Releases a reference to the object.
-- A count of the references to the object is maintained and when this count reaches zero, 
-- the memory associated with the object is freed.
releaseObject :: PtrObject -> IO Bool
releaseObject = runBool libObjectRelease

-- | Adds a reference to the object. 
-- This is intended for situations where a reference to the object needs to be maintained independently of 
-- the reference returned when the object was created.
objectAddRef :: PtrObject -> IO Bool
objectAddRef = runBool libObjectAddRef

-- | Sets the value of the element found at the specified index.
objectAppendElement :: PtrObject -> NativeTypeNum -> PtrData -> IO Bool
objectAppendElement (cxt,p) ntn pd
  = libObjectAppendElement p
    & inEnum ntn
    & inVar pd
    & outBool

-- | Creates an independent copy of an object and returns a reference to the newly created object. 
-- This reference should be released as soon as it is no longer needed.
copyObject :: PtrObject -> IO PtrObject
copyObject  p@(cxt,_)= (cxt,) <$> runVar libObjectCopy p

-- | Trims a number of elements from the end of a collection.
trimObject :: PtrObject -> Int -> IO Bool
trimObject (cxt,p) size
  = libObjectTrim p
    & inInt size
    & outBool

-- | Deletes an element from the collection. 
-- Note that the position ordinals of the remaining elements are not changed. 
-- The delete operation creates holes in the collection.
objectDeleteElementByIndex :: PtrObject -> Int -> IO Bool
objectDeleteElementByIndex (cxt,p) pos
  = libObjectDeleteElementByIndex p
    & inInt pos
    & outBool

-- | Sets the value of one of the object’s attributes.
setObjectAttributeValue :: PtrObject -> PtrObjectAttr -> DataValue -> IO Bool
setObjectAttributeValue (cxt,p) (_,poa) v = do
  (ntn, pd) <- newData v
  libObjectSetAttributeValue p poa & inEnum ntn & inVar pd & outBool

-- | Returns the value of one of the object’s attributes.
getObjectAttributeValue :: PtrObject -> PtrObjectAttr -> NativeTypeNum -> IO DataValue
getObjectAttributeValue (cxt,p) (_,poa) ntn
  = libObjectGetAttributeValue p
    & inVar poa
    & inEnum ntn
    & outValue cxt (_get ntn)

-- Returns whether an element exists at the specified index.
getObjectElementExistsByIndex :: PtrObject -> Int -> IO Bool
getObjectElementExistsByIndex (cxt,p) ind
  = libObjectGetElementExistsByIndex p
    & inInt ind
    & outValue cxt peekBool

-- | Sets the value of the element found at the specified index.
setObjectElementValueByIndex :: PtrObject -> Int -> DataValue -> IO Bool
setObjectElementValueByIndex (cxt,p) ind v = do
  (ntn, pd) <- newData v
  libObjectSetElementValueByIndex p & inInt ind & inEnum ntn & inVar pd & outBool
-- | Returns the value of the element found at the specified index.
getObjectElementValueByIndex :: PtrObject -> Int -> NativeTypeNum -> IO DataValue
getObjectElementValueByIndex (cxt,p) pos ntn
  = libObjectGetElementValueByIndex p
    & inInt pos
    & inEnum ntn
    & outValue cxt (_get ntn)

-- | Returns the first index used in a collection.
getObjectFirstIndex :: PtrObject -> IO (Maybe Int)
getObjectFirstIndex = runIndex libObjectGetFirstIndex

-- | Returns the last index used in a collection.
getObjectLastIndex :: PtrObject -> IO (Maybe Int)
getObjectLastIndex = runIndex libObjectGetLastIndex

-- | Returns the next index used in a collection following the specified index.
getObjectNextIndex :: PtrObject -> Int -> IO (Maybe Int)
getObjectNextIndex p ind = runIndex (flip libObjectGetNextIndex $ fromIntegral ind) p

-- | Returns the previous index used in a collection preceding the specified index.
getObjectPrevIndex :: PtrObject -> Int -> IO (Maybe Int)
getObjectPrevIndex p ind = runIndex (flip libObjectGetPrevIndex $ fromIntegral ind) p

-- | Returns the number of elements in a collection.
getObjectSize :: PtrObject -> IO Int
getObjectSize = runInt libObjectGetSize

-- ** ObjectAttr
-- $objectattr
-- Object attribute handles are used to represent the attributes of types such as those 
-- created by the SQL command CREATE OR REPLACE TYPE. 
-- They are created by calling the function 'getObjectTypeAttributes' 
-- and are destroyed when the last reference is released by calling the function 'releaseObjectAttr'.

-- | Returns information about the attribute.
getObjectAttrInfo :: PtrObjectAttr -> IO Data_ObjectAttrInfo
getObjectAttrInfo = runVar libObjectAttrGetInfo

-- | Adds a reference to the attribute. 
-- This is intended for situations where a reference to the attribute needs to be maintained independently of 
-- the reference returned when the attribute was created.
objectAttrAddRef :: PtrObjectAttr -> IO Bool
objectAttrAddRef = runBool libObjectAttrAddRef

-- | Releases a reference to the attribute. 
-- A count of the references to the attribute is maintained and when this count reaches zero, 
-- the memory associated with the attribute is freed.
releaseObjectAttr :: PtrObjectAttr -> IO Bool
releaseObjectAttr = runBool libObjectAttrRelease

-- ** ObjectType
-- $objecttype
-- Object type handles are used to represent types such as those created by the SQL command CREATE OR REPLACE TYPE. 
-- They are created using the function 'getObjectType'
-- or implicitly when fetching from a column containing objects by calling the function 'getQueryInfo'.
--  Object types are also retrieved when used as attributes in another object by calling the function 'getObjectAttrInfo' 
-- or as the element type of a collection by calling the function 'getObjectTypeInfo'.
-- They are destroyed when the last reference is released by calling the function 'releaseObjectType'.

-- | Adds a reference to the object type. 
-- This is intended for situations where a reference to the object type needs to be maintained independently of 
  -- the reference returned when the object type was created.
objectTypeAddRef :: PtrObjectType-> IO Bool
objectTypeAddRef = runBool libObjectTypeAddRef

-- | Releases a reference to the object type. 
-- A count of the references to the object type is maintained and when this count reaches zero, 
-- the memory associated with the object type is freed.
releaseObjectType :: PtrObjectType -> IO Bool
releaseObjectType = runBool libObjectTypeRelease

-- | Returns the list of attributes that belong to the object type.
getObjectTypeAttributes :: PtrObjectType -> Int -> IO PtrObjectAttr
getObjectTypeAttributes (cxt,p) num
  = libObjectTypeGetAttributes p
    & inInt num
    & outValue cxt (peekWithCxt cxt)

-- | Returns information about the object type.
getObjectTypeInfo :: PtrObjectType -> IO Data_ObjectTypeInfo
getObjectTypeInfo = runVar libObjectTypeGetInfo

-- * Rowid Interface
-- $rowid
-- Rowid handles are used to represent the unique identifier of a row in the database. 
-- They cannot be created or set directly but are created implicitly when a variable of type 'OracleTypeRowid' is created. 
-- They are destroyed when the last reference is released by a call to the function 'releaseRowid'.

-- | Adds a reference to the rowid. This is intended for situations where a reference to the rowid needs to be maintained 
-- independently of the reference returned when the rowid was created.
rowidAddRef :: PtrRowid -> IO Bool
rowidAddRef = runBool libRowidAddRef

-- | Releases a reference to the rowid. 
-- A count of the references to the rowid is maintained and when this count reaches zero, 
-- the memory associated with the rowid is freed.
releaseRowid :: PtrRowid -> IO Bool
releaseRowid = runBool libRowidRelease

-- | Returns the sting (base64) representation of the rowid.
rowidGetStringValue :: PtrRowid -> IO Text
rowidGetStringValue (cxt,p) = libRowidGetStringValue p & out2Value cxt peekCStrLen

-- * Var Interface
-- $var
-- Variable handles are used to represent memory areas used for transferring data to and from the database.
-- They are created by calling the function 'newVar'.
--  They are destroyed when the last reference to the variable is released by calling the function 'releaseVar'.
--  They are bound to statements by calling the function 'bindByName' or the function 'bindByPosition'.
-- They can also be used for fetching data from the database by calling the function 'define'.

-- | Returns a reference to a new variable which can be used for binding data to a statement
--  or providing a buffer for querying data from the database.
-- The reference should be released as soon as it is no longer needed.
newVar :: PtrConn       -- ^ Connection
       -> OracleTypeNum -- ^ the type of Oracle data that is to be used.
       -> NativeTypeNum -- ^  the type of native C data that is to be used.
       -> Int           -- ^ the maximum number of rows that can be fetched or bound at one time from the database, 
                        -- or the maximum number of elements that can be stored in a PL/SQL array.
       -> Int           -- ^ the maximum size of the buffer used for transferring data to/from Oracle. 
                        -- This value is only used for variables transferred as byte strings. 
                        -- Size is either in characters or bytes depending on the value of the sizeIsBytes parameter. 
                        -- If the value is in characters, internally the value will be multipled by 
                        -- the maximum number of bytes for each character and that value used instead 
                        -- when determining the necessary buffer size.
       -> Bool          -- ^ boolean value indicating if the size parameter refers to characters or bytes. 
                        -- This flag is only used if the variable refers to character data.
       -> Bool          -- ^  boolean value indicating if the variable refers to a PL/SQL array or simply to buffers used for binding or fetching data.
       -> PtrObjectType -- ^ a reference to the object type of the object that is being bound or fetched. 
                        -- This value is only used if the Oracle type is 'OracleTypeObject'.
       -> IO (PtrVar, [PtrData])
newVar (cxt,p) otn ntn maxArraySize size sizeIsBytes isArray (_,oto)
  = libConnNewVar p
    & inEnum otn
    & inEnum ntn
    & inInt maxArraySize
    & inInt size
    & inBool sizeIsBytes
    & inBool isArray
    & inVar oto
    & out2Value cxt (go cxt)
    where
      go cxt (pv,pd) = do
        v <- peek pv
        d <- peekArray (fromIntegral maxArraySize) pd
        return ((cxt,v), d)

-- | Adds a reference to the variable. 
-- This is intended for situations where a reference to the variable needs to be maintained independently of 
-- the reference returned when the variable was created.
varAddRef :: PtrVar -> IO Bool
varAddRef = runBool libVarAddRef

-- | Releases a reference to the variable. 
-- A count of the references to the variable is maintained and when this count reaches zero, 
-- the memory associated with the variable is freed.
releaseVar :: PtrVar -> IO Bool
releaseVar = runBool libVarRelease

-- | Copies the data from one variable to another variable.
copyVar :: PtrVar -> Int -> PtrVar -> Int -> IO Bool
copyVar (_,p) toPos (_,from) fromPos
  = libVarCopyData p
    & inInt toPos
    & inVar from
    & inInt fromPos
    & outBool

-- | Returns a pointer to an array of dpiData structures used for transferring data to and from the database. 
-- These structures are allocated by the variable itself and are made available when the variable is first 
-- created using the function 'newVar'. If a DML returning statement is executed, however, 
-- the number of allocated elements can change in addition to the memory location.
getVarData :: PtrVar -> IO [Data]
getVarData (cxt,p) = libVarGetData p & out2Value cxt go
  where
    go (pn, pd) = join $ peekArray <$> peekInt pn <*> peek pd

-- | Returns the number of elements in a PL/SQL index-by table if the variable was created as an array 
-- by the function 'newVar'. 
-- If the variable is one of the output bind variables of a DML returning statement, however, 
-- the value returned will correspond to the number of rows returned by the DML returning statement. 
-- In all other cases, the value returned will be the number of elements the variable was created with.
getVarElementsSize :: PtrVar -> IO Int
getVarElementsSize = runInt libVarGetNumElementsInArray

-- | Returns the size of the buffer used for one element of the array used for fetching/binding Oracle data
getVarSizeInBytes :: PtrVar -> IO Int
getVarSizeInBytes = runInt libVarGetSizeInBytes

-- | Sets the variable value to the specified byte string. 
-- In the case of the variable’s Oracle type being 'OracleTypeNumber', 
-- the byte string is converted to an Oracle number during the call to this function.
setVarFromBytes :: PtrVar -> Int -> Text -> IO Bool
setVarFromBytes (cxt,p) pos bytes
  = libVarSetFromBytes p
    & inInt pos
    & inStrLen bytes
    & outBool

-- | Sets the variable value to the specified LOB.
setVarFromLob :: PtrVar -> Int -> PtrLob -> IO Bool
setVarFromLob (cxt,p) pos (_,lob) = libVarSetFromLob p & inInt pos & inVar lob & outBool

-- | Sets the variable value to the specified object.
setVarFromObject :: PtrVar -> Int -> PtrObject -> IO Bool
setVarFromObject (_,p) pos (_,obj) = libVarSetFromObject p & inInt pos & inVar obj & outBool

-- | Sets the variable value to the specified rowid.
setVarFromRowid :: PtrVar -> Int -> PtrRowid -> IO Bool
setVarFromRowid (_,p) pos (_,row) = libVarSetFromRowid p & inInt pos & inVar row & outBool

-- | Sets the variable value to the specified statement.
setVarFromStatement :: PtrVar -> Int -> PtrStmt -> IO Bool
setVarFromStatement (_,p) pos (_,st) = libVarSetFromStmt p & inInt pos & inVar st & outBool

-- | Sets the number of elements in a PL/SQL index-by table.
setVarNumberOfElements :: PtrVar -> Int -> IO Bool
setVarNumberOfElements (_,p) num = libVarSetNumElementsInArray p & inInt num & outBool


-- * DeqOptions Interface
-- $ deqOptions
-- Dequeue option handles are used to represent the options specified when dequeuing messages using advanced queueing. 
-- They are created by calling the function 'newDeqOptions' 
-- and are destroyed by releasing the last reference by calling the function 'releaseDeqOptions'.

-- | Returns a reference to a new set of dequeue options, used in dequeuing objects from a queue. 
-- The reference should be released as soon as it is no longer needed.
newDeqOptions :: PtrConn -> IO PtrDeqOptions
newDeqOptions (cxt,p) = libConnNewDeqOptions p & outValue cxt (peekWithCxt cxt)

-- | Dequeues a message from a queue.
deqObject
  :: PtrConn         -- ^ a reference to the connection from which the message is to be dequeued
  -> Text            -- ^ the name of the queue from which the message is to be dequeued
  -> PtrDeqOptions   -- ^ a reference to the dequeue options that should be used when dequeuing the message from the queue.
  -> PtrMsgProps     -- ^ a reference to the message properties that will be populated with information from the message that is dequeued.
  -> PtrObject       -- ^ a reference to the object which will be populated with the message that is dequeued.
  -> IO (Maybe Text) -- ^ a pointer to a byte string which will be populated with the id of the message that is dequeued
deqObject (cxt,p) queueName (_,options) (_,props) (_,payload)
  = libConnDeqObject p
    & inStrLen queueName
    & inVar options
    & inVar props
    & inVar payload
    & out2Value cxt go
    where
      go ps@(p,_) | p == nullPtr = return Nothing
                  | otherwise    = Just <$> peekCStrLen ps

-- | Releases a reference to the dequeue options. 
-- A count of the references to the dequeue options is maintained and when this count reaches zero, 
-- the memory associated with the options is freed.
releaseDeqOptions :: PtrDeqOptions -> IO Bool
releaseDeqOptions = runBool libDeqOptionsRelease

-- | Adds a reference to the dequeue options. 
-- This is intended for situations where a reference to the dequeue options needs to be maintained independently of 
  -- the reference returned when the handle was created.
deqOptionsAddRef :: PtrDeqOptions -> IO Bool
deqOptionsAddRef = runBool libDeqOptionsAddRef

-- | Returns the condition that must be satisfied in order for a message to be dequeued. 
-- See function 'setDeqOptionsCondition' for more information.
getDeqOptionsCondition :: PtrDeqOptions -> IO Text
getDeqOptionsCondition = runText libDeqOptionsGetCondition

-- | Sets the condition which must be true for messages to be dequeued. 
-- The condition must be a valid boolean expression similar to the where clause of a SQL query. 
-- The expression can include conditions on message properties, user data properties and PL/SQL or SQL functions. 
-- User data properties must be prefixed with tab.user_data as a qualifier to indicate 
-- the specific column of the queue table that stores the message payload.
setDeqOptionsCondition :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsCondition = setText libDeqOptionsSetCondition

-- | Returns the name of the consumer that is dequeuing messages. 
-- See function 'setDeqOptionsConsumerName' for more information.
getDeqOptionsConsumerName :: PtrDeqOptions -> IO Text
getDeqOptionsConsumerName = runText libDeqOptionsGetConsumerName

-- | Sets the name of the consumer which will be dequeuing messages. 
-- This value should only be set if the queue is set up for multiple consumers.
setDeqOptionsConsumerName :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsConsumerName = setText libDeqOptionsSetConsumerName

-- | Returns the correlation of the message to be dequeued. 
-- See function 'setDeqOptionsCorrelation' for more information.
getDeqOptionsCorrelation :: PtrDeqOptions -> IO Text
getDeqOptionsCorrelation = runText libDeqOptionsGetCorrelation

-- | Sets the correlation of the message to be dequeued. 
-- Special pattern matching characters such as the percent sign (%) and the underscore (_) can be used. 
-- If multiple messages satisfy the pattern, the order of dequeuing is undetermined.
setDeqOptionsCorrelation :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsCorrelation = setText libDeqOptionsSetCorrelation

-- | Returns the mode that is to be used when dequeuing messages.
getDeqOptionsMode :: PtrDeqOptions -> IO DeqMode
getDeqOptionsMode (cxt,p) = libDeqOptionsGetMode p & outValue cxt peekEnum

-- | Sets the mode that is to be used when dequeuing messages.
setDeqOptionsMode :: PtrDeqOptions -> DeqMode -> IO Bool
setDeqOptionsMode (cxt,p) mdm = libDeqOptionsSetMode p & inEnum mdm & outBool

-- | Returns the identifier of the specific message that is to be dequeued.
getDeqOptionsMsgId :: PtrDeqOptions -> IO Text
getDeqOptionsMsgId = runText libDeqOptionsGetMsgId

-- | Sets the identifier of the specific message to be dequeued.
setDeqOptionsMsgId :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsMsgId = setText libDeqOptionsSetMsgId

-- | Returns the position of the message that is to be dequeued.
getDeqOptionsNavigation :: PtrDeqOptions -> IO DeqNavigation
getDeqOptionsNavigation (cxt,p) = libDeqOptionsGetNavigation p & outValue cxt peekEnum

-- | Sets the position in the queue of the message that is to be dequeued.
setDeqOptionsNavigation :: PtrDeqOptions -> DeqNavigation -> IO Bool
setDeqOptionsNavigation (cxt,p) mdm = libDeqOptionsSetNavigation p & inEnum mdm & outBool

-- | Returns the transformation of the message to be dequeued. 
-- See function 'setDeqOptionsTransformation' for more information.
getDeqOptionsTransformation :: PtrDeqOptions -> IO Text
getDeqOptionsTransformation = runText libDeqOptionsGetTransformation

-- | Sets the transformation of the message to be dequeued. 
-- The transformation is applied after the message is dequeued but before it is returned to the application. 
-- It must be created using DBMS_TRANSFORM.
setDeqOptionsTransformation :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsTransformation = setText libDeqOptionsSetTransformation

-- | Returns whether the message being dequeued is part of the current transaction or constitutes a transaction on its own.
getDeqOptionsVisibility :: PtrDeqOptions -> IO Visibility
getDeqOptionsVisibility (cxt,p) = libDeqOptionsGetVisibility p & outValue cxt peekEnum

-- | Sets whether the message being dequeued is part of the current transaction or constitutes a transaction on its own.
setDeqOptionsVisibility :: PtrDeqOptions -> Visibility -> IO Bool
setDeqOptionsVisibility (cxt,p) mdm = libDeqOptionsSetVisibility p & inEnum mdm & outBool

-- | Returns the time to wait, in seconds, for a message matching the search criteria. 
-- See function 'setDeqOptionsWait' for more information.
getDeqOptionsWait :: PtrDeqOptions -> IO Int
getDeqOptionsWait = runInt libDeqOptionsGetWait

-- | Set the time to wait, in seconds, for a message matching the search criteria.
setDeqOptionsWait :: PtrDeqOptions -> Int -> IO Bool
setDeqOptionsWait (cxt,p) wait = libDeqOptionsSetWait p & inInt wait & outBool

-- | Sets the message delivery mode that is to be used when dequeuing messages.
setDeqOptionsDeliveryMode :: PtrDeqOptions -> MessageDeliveryMode -> IO Bool
setDeqOptionsDeliveryMode (cxt,p) mdm = libDeqOptionsSetDeliveryMode p & inEnum mdm & outBool

-- * EnqOptions Interface
-- $enq
-- Enqueue option handles are used to represent the options specified when enqueuing messages using advanced queueing. 
-- They are created by calling the function 'newEnqOptions' 
-- and are destroyed by releasing the last reference by calling the function 'releaseEnqOptions'.

-- | Returns a reference to a new set of enqueue options, used in enqueuing objects into a queue. 
-- The reference should be released as soon as it is no longer needed.
newEnqOptions :: PtrConn -> IO PtrEnqOptions
newEnqOptions (cxt,p) = libConnNewEnqOptions p & outValue cxt (peekWithCxt cxt)

-- | Enqueues a message to a queue.
enqObject
  :: PtrConn         -- ^ a reference to the connection from which the message is to be enqueued
  -> Text            -- ^ the name of the queue from which the message is to be enqueued
  -> PtrEnqOptions   -- ^ a reference to the enqueue options that should be used when enqueued the message from the queue.
  -> PtrMsgProps     -- ^ a reference to the message properties that will be populated with information from the message that is enqueued.
  -> PtrObject       -- ^ a reference to the object which will be populated with the message that is enqueued.
  -> IO (Maybe Text) -- ^ a pointer to a byte string which will be populated with the id of the message that is enqueued
enqObject (cxt,p) queueName (_,options) (_,props) (_,payload)
  = libConnEnqObject p
    & inStrLen queueName
    & inVar options
    & inVar props
    & inVar payload
    & out2Value cxt go
    where
      go ps@(p,_) | p == nullPtr = return Nothing
                  | otherwise    = Just <$> peekCStrLen ps

-- | Adds a reference to the enqueue options. 
-- This is intended for situations where a reference to the enqueue options needs to be maintained independently of 
-- the reference returned when the handle was created.
enqOptionsAddRef :: PtrEnqOptions -> IO Bool
enqOptionsAddRef = runBool libEnqOptionsAddRef

-- | Releases a reference to the enqueue options. 
-- A count of the references to the enqueue options is maintained and when this count reaches zero, 
-- the memory associated with the options is freed.
releaseEnqOptions :: PtrEnqOptions -> IO Bool
releaseEnqOptions = runBool libEnqOptionsRelease

-- | Returns the transformation of the message to be enqueued. 
-- See function 'setEnqOptionsTransformation' for more information.
getEnqOptionsTransformation :: PtrEnqOptions -> IO Text
getEnqOptionsTransformation = runText libEnqOptionsGetTransformation

-- | Sets the transformation of the message to be enqueued. 
-- The transformation is applied after the message is enqueued but before it is returned to the application. 
-- It must be created using DBMS_TRANSFORM.
setEnqOptionsTransformation :: PtrEnqOptions -> Text -> IO Bool
setEnqOptionsTransformation = setText libEnqOptionsSetTransformation

-- | Returns whether the message being enqueued is part of the current transaction or constitutes a transaction on its own.
getEnqOptionsVisibility :: PtrEnqOptions -> IO Visibility
getEnqOptionsVisibility (cxt,p) = libEnqOptionsGetVisibility p & outValue cxt peekEnum

-- | Sets whether the message being enqueued is part of the current transaction or constitutes a transaction on its own.
setEnqOptionsVisibility :: PtrEnqOptions -> Visibility -> IO Bool
setEnqOptionsVisibility (cxt,p) mdm = libEnqOptionsSetVisibility p & inEnum mdm & outBool

-- | Sets the message delivery mode that is to be used when enqueuing messages.
setEnqOptionsDeliveryMode :: PtrEnqOptions -> MessageDeliveryMode -> IO Bool
setEnqOptionsDeliveryMode (cxt,p) mdm = libEnqOptionsSetDeliveryMode p & inEnum mdm & outBool


-- * MsgProps Interface
-- $ msg
-- Message properties handles are used to represent the properties of messages that are enqueued 
-- and dequeued using advanced queuing. 
-- They are created by calling the function 'newMsgProps' and are destroyed by releasing the last reference 
-- by calling the function 'releaseMsgProps'.

-- | Returns a reference to a new set of message properties, used in enqueuing and dequeuing objects in a queue. 
-- The reference should be released as soon as it is no longer needed.
newMsgProps :: PtrConn -> IO PtrMsgProps
newMsgProps (cxt,p) =libConnNewMsgProps p & outValue cxt (peekWithCxt cxt)

-- | Adds a reference to the message properties. 
-- This is intended for situations where a reference to the message properties 
-- needs to be maintained independently of the reference returned when the handle was created.
msgPropsAddRef :: PtrMsgProps -> IO Bool
msgPropsAddRef = runBool libMsgPropsAddRef

-- | Releases a reference to the message properties.
--  A count of the references to the message properties is maintained and when this count reaches zero, 
-- the memory associated with the properties is freed.
releaseMsgProps :: PtrMsgProps -> IO Bool
releaseMsgProps = runBool libMsgPropsRelease

-- | Returns the correlation supplied by the producer when the message was enqueued.
getMsgPropsCorrelation :: PtrMsgProps -> IO Text
getMsgPropsCorrelation = runText libMsgPropsGetCorrelation

-- | Sets the correlation of the message to be dequeued. 
-- Special pattern matching characters such as the percent sign (%) and the underscore (_) can be used. 
-- If multiple messages satisfy the pattern, the order of dequeuing is undetermined.
setMsgPropsCorrelation :: PtrMsgProps -> Text -> IO Bool
setMsgPropsCorrelation = setText libMsgPropsSetCorrelation

-- | Returns the number of attempts that have been made to dequeue a message.
getMsgPropsNumAttempts :: PtrMsgProps -> IO Int
getMsgPropsNumAttempts = runInt libMsgPropsGetNumAttempts

-- | Returns the number of seconds the enqueued message will be delayed.
getMsgPropsDelay :: PtrMsgProps -> IO Int
getMsgPropsDelay = runInt libMsgPropsGetDelay

-- | Sets the number of seconds to delay the message before it can be dequeued. 
-- Messages enqueued with a delay are put into the 'MsgStateWaiting' state. 
-- When the delay expires the message is put into the 'MsgStateReady' state. 
-- Dequeuing directly by message id overrides this delay specification. 
-- Note that delay processing requires the queue monitor to be started.
setMsgPropsDelay :: PtrMsgProps -> Int -> IO Bool
setMsgPropsDelay (cxt,p) delay = libMsgPropsSetDelay p & inInt delay & outBool

-- | Returns the mode that was used to deliver the message.
getMsgPropsDeliveryMode :: PtrMsgProps -> IO MessageDeliveryMode
getMsgPropsDeliveryMode (cxt,p) = libMsgPropsGetDeliveryMode p & outValue cxt peekEnum

-- | Returns the time that the message was enqueued.
getMsgPropsEnqTime :: PtrMsgProps -> IO Data_Timestamp
getMsgPropsEnqTime = runVar libMsgPropsGetEnqTime

-- | Returns the name of the queue to which the message is moved if it cannot be processed successfully.
-- See function 'setMsgPropsExceptionQ' for more information.
getMsgPropsExceptionQ :: PtrMsgProps -> IO Text
getMsgPropsExceptionQ = runText libMsgPropsGetExceptionQ

-- | Sets the name of the queue to which the message is moved if it cannot be processed successfully. 
-- Messages are moved if the number of unsuccessful dequeue attempts has reached the maximum allowed number 
-- or if the message has expired. All messages in the exception queue are in the 'MsgStateExpired' state.
setMsgPropsExceptionQ :: PtrMsgProps -> Text -> IO Bool
setMsgPropsExceptionQ = setText libMsgPropsSetExceptionQ

-- | Returns the number of seconds the message is available to be dequeued. 
-- See function 'setMsgPropsExpiration' for more information.
getMsgPropsExpiration :: PtrMsgProps -> IO Int
getMsgPropsExpiration = runInt libMsgPropsGetExpiration

-- | Sets the number of seconds the message is available to be dequeued. 
-- This value is an offset from the delay. 
-- Expiration processing requires the queue monitor to be running. 
-- Until this time elapses, the messages are in the queue in the state 'MsgStateReady'. 
-- After this time elapses messages are moved to the exception queue in the 'MsgStateExpired' state.
setMsgPropsExpiration :: PtrMsgProps -> Int -> IO Bool
setMsgPropsExpiration (cxt,p) delay = libMsgPropsSetExpiration p & inInt delay & outBool

-- | Returns the id of the message in the last queue that generated this message. 
-- See function 'setMsgPropsOriginalMsgId' for more information.
getMsgPropsOriginalMsgId :: PtrMsgProps -> IO Text
getMsgPropsOriginalMsgId = runText libMsgPropsGetOriginalMsgId

-- | Sets the id of the message in the last queue that generated this message.
setMsgPropsOriginalMsgId :: PtrMsgProps -> Text -> IO Bool
setMsgPropsOriginalMsgId = setText libMsgPropsSetOriginalMsgId

-- | Returns the priority assigned to the message. 
-- See function 'setMsgPropsPriority' for more information.
getMsgPropsPriority :: PtrMsgProps -> IO Int
getMsgPropsPriority = runInt libMsgPropsGetPriority

-- | Sets the priority assigned to the message. A smaller number indicates a higher priority. 
-- The priority can be any number, including negative numbers.
setMsgPropsPriority :: PtrMsgProps -> Int -> IO Bool
setMsgPropsPriority (cxt,p) delay = libMsgPropsSetPriority p & inInt delay & outBool

-- | Returns the state of the message at the time of dequeue.
getMsgPropsState :: PtrMsgProps -> IO MessageState
getMsgPropsState (cxt,p) = libMsgPropsGetState p & outValue cxt peekEnum

-- * Subscr Interface
-- $subscr
-- Subscription handles are used to represent subscriptions to events such as continuous query notification 
-- and object change notification. 
-- They are created by calling the function 'newSubscr' 
-- and are destroyed by calling the function 'closeSubscr' or releasing the last reference by calling 
-- the function 'releaseSubscr'.

-- | Returns a reference to a subscription which is used for requesting notifications of changes on tables or queries 
-- that are made in the database. The reference should be released as soon as it is no longer needed.
newSubscr
  :: PtrConn
  -> (Data_SubscrCreateParams -> Data_SubscrCreateParams)
  -> IO PtrSubscr
newSubscr (cxt,p)  hcmp
  = libConnNewSubscription p
    & inPtr (\c -> libContextInitSubscrCreateParams cxt c >> peek c >>= poke c . hcmp)
    & out2Value cxt (go cxt)
    where
      go cxt (p,_) = (cxt,) <$> peek p

-- | Adds a reference to the subscription. 
-- This is intended for situations where a reference to the subscription 
-- needs to be maintained independently of the reference returned when the subscription was created.
subscrAddRef :: PtrSubscr -> IO Bool
subscrAddRef = runBool libSubscrAddRef

-- | Closes the subscription now, rather than when the last reference is released. 
-- This deregisters it so that notifications will no longer be sent.
closeSubscr :: PtrSubscr -> IO Bool
closeSubscr = runBool libSubscrClose

-- | Releases a reference to the subscription. 
-- A count of the references to the subscription is maintained and when this count reaches zero, 
-- the memory associated with the subscription is freed. 
-- The subscription is also deregistered so that notifications are no longer sent, 
-- if this was not already done using the function 'closeSubscr'.
releaseSubscr :: PtrSubscr -> IO Bool
releaseSubscr = runBool libSubscrRelease

-- | Prepares a statement for registration on the subscription. 
-- The statement is then registered by calling the function 'prepareStatement'. 
-- The reference to the statement that is returned should be released as soon as it is no longer needed.
subscrPrepareStatement 
  :: PtrSubscr  -- ^  a reference to the subscription on which the statement is to be prepared for registration.
  -> Text       -- ^ the SQL that is to be prepared
  -> IO PtrStmt -- ^ a reference to the statement that was prepared
subscrPrepareStatement (cxt,p) sql
  = libSubscrPrepareStmt p 
    & inStrLen sql
    & outValue cxt (peekWithCxt cxt)
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
-- These can be standalone connections created by calling the function 'createConnetion'.
-- They can be closed by calling the function 'closeConnection' or releasing the last reference
-- to the connection by calling the function 'releaseConnection'.
-- Connection handles are used to create all handles other than session pools and context handles.


-- | Creates a standalone connection to a database or acquires a connection from a session pool and returns a reference to the connection.
createConnection :: PtrContext -- ^ Context
                 -> Text -- ^ the name of the user used for authenticating the user
                 -> Text -- ^  the password to use for authenticating the user
                 -> Text -- ^ he connect string identifying the database to which a connection is to be established
                 -> (Data_CommonCreateParams -> Data_CommonCreateParams) -- ^ custom 'Data_CommonCreateParams'
                 -> (Data_ConnCreateParams   -> Data_ConnCreateParams) -- ^ custom 'Data_ConnCreateParams'
                 -> IO PtrConn
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

-- | commit
commitConnection :: PtrConn -> IO Bool
commitConnection = runBool libConnCommit

-- | rollback
rollbackConnection :: PtrConn -> IO Bool
rollbackConnection = runBool libConnRollback

-- | Pings the database to verify that the connection is still alive.
pingConnection :: PtrConn -> IO Bool
pingConnection = runBool libConnPing

-- | with connection
withConnection
  :: PtrContext -- ^ Context
  -> Text -- ^ Username
  -> Text -- ^ Password
  -> Text -- ^ Connection String
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
beginTransaction
  :: PtrConn    -- ^ Connection
  -> Int64      -- ^ formatId
  -> Text -- ^ transactionId
  -> Text -- ^ branchId
  -> IO Bool
beginTransaction p formatId transId branchId
  = libConnBeginDistribTrans (snd p) (fromIntegral formatId)
    & inStrLen transId
    & inStrLen branchId
    & outBool

-- | Prepares a distributed transaction for commit.
-- This function should only be called after 'beginTransaction' is called and before 'commitConnection' is called.
prepareTransaction :: PtrConn -> IO Bool
prepareTransaction (cxt,p) = libConnPrepareDistribTrans p & outValue cxt peekBool

-- ** Information from Connection

getCurrentSchema :: PtrConn -> IO Text
getCurrentSchema = runText libConnGetCurrentSchema

setCurrentSchema :: PtrConn -> Text -> IO Bool
setCurrentSchema = setText libConnSetCurrentSchema

getEdition :: PtrConn -> IO Text
getEdition = runText libConnGetEdition

getExternalName :: PtrConn -> IO Text
getExternalName = runText libConnGetExternalName

setExternalName :: PtrConn -> Text -> IO Bool
setExternalName = setText libConnSetExternalName

getInternalName :: PtrConn -> IO Text
getInternalName = runText libConnGetInternalName

setInternalName :: PtrConn -> Text -> IO Bool
setInternalName = setText libConnSetInternalName

getLTXID :: PtrConn -> IO Text
getLTXID = runText libConnGetLTXID

getServerVersion :: PtrConn -> IO (Text, Data_VersionInfo)
getServerVersion (cxt,p) = libConnGetServerVersion p & out3Value cxt go
  where
    go (pl,pv) = (,) <$> peekCStrLen pl <*> peek pv

getObjectType :: PtrConn -> Text -> IO PtrObjectType
getObjectType (cxt,p) name
  = libConnGetObjectType p
    & inStrLen name
    & outValue cxt (peekWithCxt cxt)

getEncodingInfo :: PtrConn -> IO Data_EncodingInfo
getEncodingInfo = runVar libConnGetEncodingInfo

getStmtCacheSize :: PtrConn -> IO Int
getStmtCacheSize = runInt libConnGetStmtCacheSize

setStmtCacheSize :: PtrConn -> Int -> IO Bool
setStmtCacheSize (cxt,p) size = libConnSetStmtCacheSize p & inInt size & outBool

setClientInfo :: PtrConn -> Text -> IO Bool
setClientInfo = setText libConnSetClientInfo

setClientIdentifier :: PtrConn -> Text -> IO Bool
setClientIdentifier = setText libConnSetClientIdentifier

setAction :: PtrConn -> Text -> IO Bool
setAction = setText libConnSetAction

setDbOp :: PtrConn -> Text -> IO Bool
setDbOp = setText libConnSetDbOp

setConnMode :: PtrConn -> Text -> IO Bool
setConnMode = setText libConnSetModule

getHandler :: PtrConn -> IO (Ptr ())
getHandler = runVar libConnGetHandle

-- ** Connection Management


connectionAddRef :: PtrConn -> IO Bool
connectionAddRef = runBool libConnAddRef

breakException :: PtrConn -> IO Bool
breakException = runBool libConnBreakExecution

changePassword
  :: PtrConn -- ^ Connection
  -> Text    -- ^ the name of the user whose password is to be changed
  -> Text    -- ^ the old password of the user whose password is to be changed
  -> Text    -- ^  the new password of the user whose password is to be changed
  -> IO Bool
changePassword (cxt,p) username oldPassword newPassword
  = libConnChangePassword p
    & inStrLen username
    & inStrLen oldPassword
    & inStrLen newPassword
    & outBool

shutdownDatabase :: PtrConn -> ShutdownMode -> IO Bool
shutdownDatabase (cxt,p) sm = libConnShutdownDatabase p & inEnum sm & outBool

startupDatabase :: PtrConn -> StartupMode -> IO Bool
startupDatabase (cxt,p) sm = libConnStartupDatabase p & inEnum sm & outBool


-- Data Interface
-- libDataGetBool
-- libDataGetBytes
-- libDataGetDouble
-- libDataGetFloat
-- libDataGetInt
-- libDataGetIntervalDS
-- libDataGetIntervalYM
-- libDataGetLOB
-- libDataGetObject
-- libDataGetStmt
-- libDataGetTimestamp
-- libDataGetUint
-- libDataSetBool
-- libDataSetBytes
-- libDataSetDouble
-- libDataSetFloat
-- libDataSetInt
-- libDataSetIntervalDS
-- libDataSetIntervalYM
-- libDataSetLOB
-- libDataSetObject
-- libDataSetStmt
-- libDataSetTimestamp
-- libDataSetUint

-- * ConnectionPool Interace
-- $pool
-- Pool handles are used to represent session pools.
-- They are created using the function 'createPool' and can be closed by calling the function 'closePool'
-- or releasing the last reference to the pool by calling the function 'releasePool'.
-- Pools can be used to create connections by calling the function 'acquiredConnection'.

-- | Acquires a connection from the pool and returns a reference to it. This reference should be released as soon as it is no longer needed.
acquiredConnection :: PtrPool -> IO PtrConn
acquiredConnection (cxt,p) = libPoolAcquireConnection p nullPtr 0 nullPtr 0 nullPtr & outValue cxt (peekWithCxt cxt)

poolAddRef :: PtrPool -> IO Bool
poolAddRef = runBool libPoolAddRef

-- | Creates a session pool which creates and maintains a group of stateless sessions to the database.
--  The main benefit of session pooling is performance since making a connection to the database is a time-consuming activity,
-- especially when the database is remote.
createPool
  :: PtrContext -- ^ Context
  -> Text -- ^ the name of the user used for authenticating the user
  -> Text -- ^  the password to use for authenticating the user
  -> Text -- ^ he connect string identifying the database to which a connection is to be established
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
  -> Text -- ^ Username
  -> Text -- ^ Password
  -> Text -- ^ Connection String
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

withPoolConnection :: PtrPool -> (PtrConn -> IO a) -> IO a
withPoolConnection p = bracket (acquiredConnection p) releaseConnection

getPoolBusyCount :: PtrPool -> IO Int
getPoolBusyCount = runInt libPoolGetBusyCount

getPoolEncodingInfo :: PtrPool -> IO Data_EncodingInfo
getPoolEncodingInfo = runVar libPoolGetEncodingInfo

getPoolMode :: PtrPool -> IO PoolGetMode
getPoolMode (cxt,p) = libPoolGetGetMode p & outValue cxt peekEnum

getPoolMaxLifetimeSession :: PtrPool -> IO Int
getPoolMaxLifetimeSession = runInt libPoolGetMaxLifetimeSession

getPoolOpenCount :: PtrPool -> IO Int
getPoolOpenCount = runInt libPoolGetOpenCount

getPoolStmtCacheSize :: PtrPool -> IO Int
getPoolStmtCacheSize = runInt libPoolGetStmtCacheSize

getPoolTimeout :: PtrPool -> IO Int
getPoolTimeout = runInt libPoolGetTimeout

setPoolGetMode :: PtrPool -> PoolGetMode -> IO Bool
setPoolGetMode (cxt,p) mode = libPoolSetGetMode p & inEnum mode & outBool

setPoolMaxLifetimeSession :: PtrPool -> Int -> IO Bool
setPoolMaxLifetimeSession (cxt,p) maxLifetimeSession
  = libPoolSetMaxLifetimeSession p
    & inInt maxLifetimeSession
    & outBool

setPoolStmtCacheSize :: PtrPool -> Int -> IO Bool
setPoolStmtCacheSize (cxt,p) stmtCacheSize
  = libPoolSetStmtCacheSize p
    & inInt stmtCacheSize
    & outBool

setPoolTimeout :: PtrPool -> Int -> IO Bool
setPoolTimeout (cxt,p) timeout
  = libPoolSetTimeout p
    & inInt timeout
    & outBool

-- * Statement Interface
-- $statement
-- Statement handles are used to represent statements of all types (queries, DML, DDL and PL/SQL).
--  They are created by calling the function 'createStatement'.
-- They are also created implicitly when a variable of type 'OracleTypeStmt' is created.
--  Statement handles can be closed by calling the function 'closeStatement'
-- or by releasing the last reference to the statement by calling the function 'releaseStatement'.

-- | Returns a reference to a statement prepared for execution.
-- The reference should be released as soon as it is no longer needed.
createStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ scrollable
  -> Text       -- ^ SQL String, not allow to use multi lines or semicolon as end of sql.
                -- use 'normalize' use normalize sql text.
  -> IO PtrStmt
createStatement (cxt,p) scrollable sql
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

withStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ scrollable
  -> Text -- ^ SQL String
  -> (PtrStmt -> IO a)
  -> IO a
withStatement p scrollable sql f
  = bracket
      (createStatement p scrollable sql)
      releaseStatement
      $ \s -> do a <- f s
                 a `seq` return a

-- | Scrolls the statement to the position in the cursor specified by the mode and offset.
scrollStatement :: PtrStmt -> FetchMode -> Int -> Int -> IO Bool
scrollStatement (cxt,p) mode offset rowOffset = libStmtScroll p & inEnum mode & inInt offset & inInt rowOffset & outBool

statementAddRef :: PtrStmt -> IO Bool
statementAddRef = runBool libStmtAddRef

-- ** Statement Bind Vars

-- | Binds a variable to a named placeholder in the statement.
-- A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same name.
bindByName :: PtrStmt -> Text -> PtrVar -> IO Bool
bindByName (cxt,p) name (_,var)
  = libStmtBindByName p
    & inStrLen name
    & inVar var
    & outBool

-- | Binds a variable to a placeholder in the statement by position.
--  A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same position.
bindByPosition :: PtrStmt -> Int -> PtrVar -> IO Bool
bindByPosition (cxt,p) pos (_,var)
  = libStmtBindByPos p
    & inInt pos
    & inVar var
    & outBool

-- | Binds a value to a named placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same name.
bindValueByName :: PtrStmt -> Text -> NativeTypeNum -> PtrData -> IO Bool
bindValueByName (cxt,p) name ntn dt
  = libStmtBindValueByName p
    & inStrLen name
    & inEnum ntn
    & inVar dt
    & outBool

-- | Binds a value to a placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same position.
bindValueByPosition :: PtrStmt -> Int -> NativeTypeNum -> PtrData -> IO Bool
bindValueByPosition (cxt,p) pos ntn dt
  = libStmtBindValueByPos p
    & inInt pos
    & inEnum ntn
    & inVar dt
    & outBool

-- | Defines the variable that will be used to fetch rows from the statement.
-- A reference to the variable will be retained until the next define is performed on the same position
-- or the statement is closed.
define :: PtrStmt -> Int -> PtrVar -> IO Bool
define (cxt,p) pos (_,var)
  = libStmtDefine p
    & inInt pos
    & inVar var
    & outBool

-- | Defines the type of data that will be used to fetch rows from the statement.
-- This is intended for use with the function 'getQueryValue', when the default data type
-- derived from the column metadata needs to be overridden by the application.
-- Internally, a variable is created with the specified data type and size.
defineValue :: PtrStmt -> Int -> OracleTypeNum -> NativeTypeNum -> Int -> Bool -> PtrObjectType -> IO Bool
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

getStatementInfo :: PtrStmt -> IO Data_StmtInfo
getStatementInfo = runVar libStmtGetInfo

getFetchArraySize :: PtrStmt -> IO Int
getFetchArraySize = runInt libStmtGetFetchArraySize

setFetchArraySize :: PtrStmt -> Int -> IO Bool
setFetchArraySize (cxt,p) pos
  = libStmtSetFetchArraySize p
    & inInt pos
    & outBool

getImplicitResult :: PtrStmt -> IO (Maybe PtrStmt)
getImplicitResult p@(cxt,_) = do
  ps <- runMaybeVar libStmtGetImplicitResult p
  return $ fmap (cxt,) ps

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

lobAddRef :: PtrLob -> IO Bool
lobAddRef = runBool libLobAddRef

newTempLob :: PtrConn -> OracleTypeNum -> IO PtrLob
newTempLob (cxt,p) otn
  = libConnNewTempLob p
    & inEnum otn
    & outValue cxt (peekWithCxt cxt)

closeLob :: PtrLob -> IO Bool
closeLob = runBool libLobClose

closeLobResource :: PtrLob -> IO Bool
closeLobResource = runBool libLobCloseResource

copyLob :: PtrLob -> IO PtrLob
copyLob p@(cxt,_)= (cxt,) <$> runVar libLobCopy p

flushLob :: PtrLob -> IO Bool
flushLob = runBool libLobFlushBuffer

getLobBufferSize :: PtrLob -> Word64 -> IO Word64
getLobBufferSize (cxt,p) size
  = libLobGetBufferSize p
    & inInt size
    & outValue cxt peekInt

getLobChunkSize :: PtrLob -> IO Int64
getLobChunkSize = runInt libLobGetChunkSize

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

setLobDirectoryAndFileName :: PtrLob -> (FilePath, String) -> IO Bool
setLobDirectoryAndFileName (cxt,p) (fp, name)
  = libLobSetDirectoryAndFileName p
    & inStrLen fp
    & inStrLen name
    & outBool

lobFileExists :: PtrLob -> IO Bool
lobFileExists (cxt,p) = libLobGetFileExists p & outValue cxt peekBool

isLobResourceOpen :: PtrLob -> IO Bool
isLobResourceOpen (cxt,p) = libLobGetIsResourceOpen p & outValue cxt peekBool

getLobSize :: PtrLob -> IO Int64
getLobSize = runInt libLobGetSize

openLobResource :: PtrLob -> IO Bool
openLobResource = runBool libLobOpenResource

releaseLob :: PtrLob -> IO Bool
releaseLob = runBool libLobRelease

trimLob :: PtrLob -> Int64 -> IO Bool
trimLob (cxt,p) size = libLobTrim p & inInt size & outBool

setLobFromBytes :: PtrLob -> Text -> IO Bool
setLobFromBytes (cxt,p) buff
  = libLobSetFromBytes p
    & inStrLen buff
    & outBool

type BufferSize = Int64

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

writeLobBytes :: PtrLob -> PageOffset -> Text -> IO Bool
writeLobBytes (cxt,p) size buff
  = libLobWriteBytes p
    & inInt size
    & inStrLen buff
    & outBool

-- * Object Interface

objectAddRef :: PtrObject -> IO Bool
objectAddRef = runBool libObjectAddRef

objectAppendElement :: PtrObject -> NativeTypeNum -> PtrData -> IO Bool
objectAppendElement (cxt,p) ntn pd
  = libObjectAppendElement p
    & inEnum ntn
    & inVar pd
    & outBool

copyObject :: PtrObject -> IO PtrObject
copyObject  p@(cxt,_)= (cxt,) <$> runVar libObjectCopy p

releaseObject :: PtrObject -> IO Bool
releaseObject = runBool libObjectRelease

trimObject :: PtrObject -> Int -> IO Bool
trimObject (cxt,p) size
  = libObjectTrim p
    & inInt size
    & outBool

objectDeleteElementByIndex :: PtrObject -> Int -> IO Bool
objectDeleteElementByIndex (cxt,p) pos
  = libObjectDeleteElementByIndex p
    & inInt pos
    & outBool

objectSetAttributeValue :: PtrObject -> PtrObjectAttr -> DataValue -> IO Bool
objectSetAttributeValue (cxt,p) (_,poa) v = do
  (ntn, pd) <- newData v
  libObjectSetAttributeValue p poa & inEnum ntn & inVar pd & outBool

objectGetAttributeValue :: PtrObject -> PtrObjectAttr -> NativeTypeNum -> IO DataValue
objectGetAttributeValue (cxt,p) (_,poa) ntn
  = libObjectGetAttributeValue p
    & inVar poa
    & inEnum ntn
    & outValue cxt (_get ntn)

objectGetElementExistsByIndex :: PtrObject -> Int -> IO Bool
objectGetElementExistsByIndex (cxt,p) ind
  = libObjectGetElementExistsByIndex p
    & inInt ind
    & outValue cxt peekBool

objectSetElementValueByIndex :: PtrObject -> Int -> DataValue -> IO Bool
objectSetElementValueByIndex (cxt,p) ind v = do
  (ntn, pd) <- newData v
  libObjectSetElementValueByIndex p & inInt ind & inEnum ntn & inVar pd & outBool

objectGetElementValueByIndex :: PtrObject -> Int -> NativeTypeNum -> IO DataValue
objectGetElementValueByIndex (cxt,p) pos ntn
  = libObjectGetElementValueByIndex p
    & inInt pos
    & inEnum ntn
    & outValue cxt (_get ntn)

objectGetFirstIndex :: PtrObject -> IO (Maybe Int)
objectGetFirstIndex = runIndex libObjectGetFirstIndex

objectGetLastIndex :: PtrObject -> IO (Maybe Int)
objectGetLastIndex = runIndex libObjectGetLastIndex

objectGetNextIndex :: PtrObject -> Int -> IO (Maybe Int)
objectGetNextIndex p ind = runIndex (flip libObjectGetNextIndex $ fromIntegral ind) p

objectGetPrevIndex :: PtrObject -> Int -> IO (Maybe Int)
objectGetPrevIndex p ind = runIndex (flip libObjectGetPrevIndex $ fromIntegral ind) p

getObjectSize :: PtrObject -> IO Int
getObjectSize = runInt libObjectGetSize

getObjectAttrInfo :: PtrObjectAttr -> IO Data_ObjectAttrInfo
getObjectAttrInfo = runVar libObjectAttrGetInfo

objectAttrAddRef :: PtrObjectAttr -> IO Bool
objectAttrAddRef = runBool libObjectAttrAddRef

releaseObjectAttr :: PtrObjectAttr -> IO Bool
releaseObjectAttr = runBool libObjectAttrRelease

objectTypeAddRef :: PtrObjectType-> IO Bool
objectTypeAddRef = runBool libObjectTypeAddRef

createObjectByType :: PtrObjectType -> IO PtrObject
createObjectByType p@(cxt,_)= (cxt,) <$> runVar libObjectTypeCreateObject p

objectTypeGetAttributes :: PtrObjectType -> Int -> IO PtrObjectAttr
objectTypeGetAttributes (cxt,p) num
  = libObjectTypeGetAttributes p
    & inInt num
    & outValue cxt (peekWithCxt cxt)

objectTypeGetInfo :: PtrObjectType -> IO Data_ObjectTypeInfo
objectTypeGetInfo = runVar libObjectTypeGetInfo

releaseObjectType :: PtrObjectType -> IO Bool
releaseObjectType = runBool libObjectTypeRelease

-- * Rowid Interface

rowidAddRef :: PtrRowid -> IO Bool
rowidAddRef = runBool libRowidAddRef

releaseRowid :: PtrRowid -> IO Bool
releaseRowid = runBool libRowidRelease

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
       -> OracleTypeNum -- ^ Oracle type enum
       -> NativeTypeNum -- ^ Native type enum
       -> Int           -- ^ maxArraySize
       -> Int           -- ^ size
       -> Bool          -- ^ sizeIsBytes
       -> Bool          -- ^ isArray
       -> PtrObjectType -- ^ Object type
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


varAddRef :: PtrVar -> IO Bool
varAddRef = runBool libVarAddRef

copyVar :: PtrVar -> Int -> PtrVar -> Int -> IO Bool
copyVar (_,p) toPos (_,from) fromPos
  = libVarCopyData p
    & inInt toPos
    & inVar from
    & inInt fromPos
    & outBool

varGetData :: PtrVar -> IO [Data]
varGetData (cxt,p) = libVarGetData p & out2Value cxt go
  where
    go (pn, pd) = join $ peekArray <$> peekInt pn <*> peek pd

varGetNumberOfElements :: PtrVar -> IO Int
varGetNumberOfElements = runInt libVarGetNumElementsInArray

varGetSizeInBytes :: PtrVar -> IO Int
varGetSizeInBytes = runInt libVarGetSizeInBytes

releaseVar :: PtrVar -> IO Bool
releaseVar = runBool libVarRelease

setVarFromBytes :: PtrVar -> Int -> Text -> IO Bool
setVarFromBytes (cxt,p) pos bytes
  = libVarSetFromBytes p
    & inInt pos
    & inStrLen bytes
    & outBool

setVarFromLob :: PtrVar -> Int -> PtrLob -> IO Bool
setVarFromLob (cxt,p) pos (_,lob) = libVarSetFromLob p & inInt pos & inVar lob & outBool

setVarFromObject :: PtrVar -> Int -> PtrObject -> IO Bool
setVarFromObject (_,p) pos (_,obj) = libVarSetFromObject p & inInt pos & inVar obj & outBool

setVarFromRowid :: PtrVar -> Int -> PtrRowid -> IO Bool
setVarFromRowid (_,p) pos (_,row) = libVarSetFromRowid p & inInt pos & inVar row & outBool

setVarFromStatement :: PtrVar -> Int -> PtrStmt -> IO Bool
setVarFromStatement (_,p) pos (_,st) = libVarSetFromStmt p & inInt pos & inVar st & outBool

setVarNumberOfElements :: PtrVar -> Int -> IO Bool
setVarNumberOfElements (_,p) num = libVarSetNumElementsInArray p & inInt num & outBool


-- * DeqOptions Interface

newDeqOptions :: PtrConn -> IO PtrDeqOptions
newDeqOptions (cxt,p) = libConnNewDeqOptions p & outValue cxt (peekWithCxt cxt)

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


deqOptionsAddRef :: PtrDeqOptions -> IO Bool
deqOptionsAddRef = runBool libDeqOptionsAddRef

getDeqOptionsCondition :: PtrDeqOptions -> IO Text
getDeqOptionsCondition = runText libDeqOptionsGetCondition

setDeqOptionsCondition :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsCondition = setText libDeqOptionsSetCondition

getDeqOptionsConsumerName :: PtrDeqOptions -> IO Text
getDeqOptionsConsumerName = runText libDeqOptionsGetConsumerName

setDeqOptionsConsumerName :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsConsumerName = setText libDeqOptionsSetConsumerName

getDeqOptionsCorrelation :: PtrDeqOptions -> IO Text
getDeqOptionsCorrelation = runText libDeqOptionsGetCorrelation

setDeqOptionsCorrelation :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsCorrelation = setText libDeqOptionsSetCorrelation

getDeqOptionsMode :: PtrDeqOptions -> IO DeqMode
getDeqOptionsMode (cxt,p) = libDeqOptionsGetMode p & outValue cxt peekEnum

setDeqOptionsMode :: PtrDeqOptions -> DeqMode -> IO Bool
setDeqOptionsMode (cxt,p) mdm = libDeqOptionsSetMode p & inEnum mdm & outBool

getDeqOptionsMsgId :: PtrDeqOptions -> IO Text
getDeqOptionsMsgId = runText libDeqOptionsGetMsgId

setDeqOptionsMsgId :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsMsgId = setText libDeqOptionsSetMsgId

getDeqOptionsNavigation :: PtrDeqOptions -> IO DeqNavigation
getDeqOptionsNavigation (cxt,p) = libDeqOptionsGetNavigation p & outValue cxt peekEnum

setDeqOptionsNavigation :: PtrDeqOptions -> DeqNavigation -> IO Bool
setDeqOptionsNavigation (cxt,p) mdm = libDeqOptionsSetNavigation p & inEnum mdm & outBool

getDeqOptionsTransformation :: PtrDeqOptions -> IO Text
getDeqOptionsTransformation = runText libDeqOptionsGetTransformation

setDeqOptionsTransformation :: PtrDeqOptions -> Text -> IO Bool
setDeqOptionsTransformation = setText libDeqOptionsSetTransformation

getDeqOptionsVisibility :: PtrDeqOptions -> IO Visibility
getDeqOptionsVisibility (cxt,p) = libDeqOptionsGetVisibility p & outValue cxt peekEnum

setDeqOptionsVisibility :: PtrDeqOptions -> Visibility -> IO Bool
setDeqOptionsVisibility (cxt,p) mdm = libDeqOptionsSetVisibility p & inEnum mdm & outBool

getDeqOptionsWait :: PtrDeqOptions -> IO Int
getDeqOptionsWait = runInt libDeqOptionsGetWait

setDeqOptionsWait :: PtrDeqOptions -> Int -> IO Bool
setDeqOptionsWait (cxt,p) wait = libDeqOptionsSetWait p & inInt wait & outBool

setDeqOptionsDeliveryMode :: PtrDeqOptions -> MessageDeliveryMode -> IO Bool
setDeqOptionsDeliveryMode (cxt,p) mdm = libDeqOptionsSetDeliveryMode p & inEnum mdm & outBool

releaseDeqOptions :: PtrDeqOptions -> IO Bool
releaseDeqOptions = runBool libDeqOptionsRelease

-- * EnqOptions Interface

newEnqOptions :: PtrConn -> IO PtrEnqOptions
newEnqOptions (cxt,p) = libConnNewEnqOptions p & outValue cxt (peekWithCxt cxt)

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


enqOptionsAddRef :: PtrEnqOptions -> IO Bool
enqOptionsAddRef = runBool libEnqOptionsAddRef

getEnqOptionsTransformation :: PtrEnqOptions -> IO Text
getEnqOptionsTransformation = runText libEnqOptionsGetTransformation

setEnqOptionsTransformation :: PtrEnqOptions -> Text -> IO Bool
setEnqOptionsTransformation = setText libEnqOptionsSetTransformation

releaseEnqOptions :: PtrEnqOptions -> IO Bool
releaseEnqOptions = runBool libEnqOptionsRelease

getEnqOptionsVisibility :: PtrEnqOptions -> IO Visibility
getEnqOptionsVisibility (cxt,p) = libEnqOptionsGetVisibility p & outValue cxt peekEnum

setEnqOptionsVisibility :: PtrEnqOptions -> Visibility -> IO Bool
setEnqOptionsVisibility (cxt,p) mdm = libEnqOptionsSetVisibility p & inEnum mdm & outBool

setEnqOptionsDeliveryMode :: PtrEnqOptions -> MessageDeliveryMode -> IO Bool
setEnqOptionsDeliveryMode (cxt,p) mdm = libEnqOptionsSetDeliveryMode p & inEnum mdm & outBool


-- * MsgProps

newMsgProps :: PtrConn -> IO PtrMsgProps
newMsgProps (cxt,p) =libConnNewMsgProps p & outValue cxt (peekWithCxt cxt)

msgPropsAddRef :: PtrMsgProps -> IO Bool
msgPropsAddRef = runBool libMsgPropsAddRef

releaseMsgProps :: PtrMsgProps -> IO Bool
releaseMsgProps = runBool libMsgPropsRelease

getMsgPropsCorrelation :: PtrMsgProps -> IO Text
getMsgPropsCorrelation = runText libMsgPropsGetCorrelation

setMsgPropsCorrelation :: PtrMsgProps -> Text -> IO Bool
setMsgPropsCorrelation = setText libMsgPropsSetCorrelation

getMsgPropsNumAttempts :: PtrMsgProps -> IO Int
getMsgPropsNumAttempts = runInt libMsgPropsGetNumAttempts

getMsgPropsDelay :: PtrMsgProps -> IO Int
getMsgPropsDelay = runInt libMsgPropsGetDelay

setMsgPropsDelay :: PtrMsgProps -> Int -> IO Bool
setMsgPropsDelay (cxt,p) delay = libMsgPropsSetDelay p & inInt delay & outBool

getMsgPropsDeliveryMode :: PtrMsgProps -> IO MessageDeliveryMode
getMsgPropsDeliveryMode (cxt,p) = libMsgPropsGetDeliveryMode p & outValue cxt peekEnum

getMsgPropsEnqTime :: PtrMsgProps -> IO Data_Timestamp
getMsgPropsEnqTime = runVar libMsgPropsGetEnqTime

getMsgPropsExceptionQ :: PtrMsgProps -> IO Text
getMsgPropsExceptionQ = runText libMsgPropsGetExceptionQ

setMsgPropsExceptionQ :: PtrMsgProps -> Text -> IO Bool
setMsgPropsExceptionQ = setText libMsgPropsSetExceptionQ

getMsgPropsExpiration :: PtrMsgProps -> IO Int
getMsgPropsExpiration = runInt libMsgPropsGetExpiration

setMsgPropsExpiration :: PtrMsgProps -> Int -> IO Bool
setMsgPropsExpiration (cxt,p) delay = libMsgPropsSetExpiration p & inInt delay & outBool

getMsgPropsOriginalMsgId :: PtrMsgProps -> IO Text
getMsgPropsOriginalMsgId = runText libMsgPropsGetOriginalMsgId

setMsgPropsOriginalMsgId :: PtrMsgProps -> Text -> IO Bool
setMsgPropsOriginalMsgId = setText libMsgPropsSetOriginalMsgId

getMsgPropsPriority :: PtrMsgProps -> IO Int
getMsgPropsPriority = runInt libMsgPropsGetPriority

setMsgPropsPriority :: PtrMsgProps -> Int -> IO Bool
setMsgPropsPriority (cxt,p) delay = libMsgPropsSetPriority p & inInt delay & outBool

getMsgPropsState :: PtrMsgProps -> IO MessageState
getMsgPropsState (cxt,p) = libMsgPropsGetState p & outValue cxt peekEnum

-- * Interface Subscr

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

subscrAddRef :: PtrSubscr -> IO Bool
subscrAddRef = runBool libSubscrAddRef

closeSubscr :: PtrSubscr -> IO Bool
closeSubscr = runBool libSubscrClose

releaseSubscr :: PtrSubscr -> IO Bool
releaseSubscr = runBool libSubscrRelease


-- libSubscrAddRef
--
-- libSubscrPrepareStmt
--



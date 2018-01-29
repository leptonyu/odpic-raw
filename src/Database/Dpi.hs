{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  ) where

import           Database.Dpi.Internal
import           Database.Dpi.Prelude
import           Database.Dpi.Util

import           Control.Exception
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
createContext = libContextCreate & inInt majorVersion & inInt minorVersion & uncurry & outPtrs go
  where
    go (pc,pe) i = if isOk i then peek pc else peek pe >>= throw . ErrorInfoException

-- | Destroys the context that was earlier created with the function 'createContext'.
destroyContext :: PtrContext -> IO Bool
destroyContext = runOk libContextDestroy

-- | With Context, 'PtrContext' will be destroyed after run
withContext :: (PtrContext -> IO a) -> IO a
withContext = bracket createContext destroyContext

-- ** Information from Context

-- | Return information about the version of the Oracle Client that is being used.
getClientVersion :: PtrContext -> IO Data_VersionInfo
getClientVersion p = libContextGetClientVersion p & outPtrs (go p)
  where
    go p pc i = if isOk i then peek pc else throwContextError p

-- | Returns error information for the last error that was raised by the library.
-- This function must be called with the same thread that generated the error.
--  It must also be called before any other ODPI-C library calls are made on
-- the calling thread since the error information specific to that thread is cleared
--  at the start of every ODPI-C function call.
getContextError :: PtrContext -> IO Data_ErrorInfo
getContextError p = alloca $ \pe -> libContextGetError p pe >> peek pe

throwContextError :: PtrContext -> IO a
throwContextError p = getContextError p >>= throw . ErrorInfoException

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
                 -> IO PtrConn
createConnection cxt username password connstr hcmp
  = libConnCreate cxt
    & inStrLen username
    & inStrLen password
    & inStrLen connstr
    & inPtr (\c -> libContextInitCommonCreateParams cxt c >> peek c >>= poke c . hcmp)
    & inPtr (libContextInitConnCreateParams   cxt)
    & outPtrs (go cxt)
    where
      go p pc i = if isOk i then peek pc else throwContextError p

-- | Closes the connection and makes it unusable for further activity. close connection, but not release resource, plese use 'releaseConnection' to release and close connection
closeConnection :: ConnCloseMode -> PtrConn -> IO Bool
closeConnection mode p = isOk <$> libConnClose p (fe mode) nullPtr 0

-- | Releases a reference to the connection. A count of the references to the connection is maintained
-- and when this count reaches zero, the memory associated with the connection is freed and
-- the connection is closed or released back to the session pool if that has not already taken place
--  using the function 'closeConnection'.
releaseConnection :: PtrConn -> IO Bool
releaseConnection = runOk libConnRelease

-- | commit
commitConnection :: PtrConn -> IO Bool
commitConnection = runOk libConnCommit

-- | rollback
rollbackConnection :: PtrConn -> IO Bool
rollbackConnection = runOk libConnRollback

-- | Pings the database to verify that the connection is still alive.
pingConnection :: PtrConn -> IO Bool
pingConnection = runOk libConnPing

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
      (createConnection p username password connstr (set lang nchar))
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
  = libConnBeginDistribTrans p (fromIntegral formatId)
    & inStrLen transId
    & inStrLen branchId
    & fmap isOk

-- | Prepares a distributed transaction for commit.
-- This function should only be called after 'beginTransaction' is called and before 'commitConnection' is called.
prepareTransaction :: PtrConn -> IO Bool
prepareTransaction p = libConnPrepareDistribTrans p & outPtrs (checkOk "prepareTransaction" peekBool)

-- ** Information from Connection

getCurrentSchema :: PtrConn -> IO Text
getCurrentSchema = _getConn "getCurrentSchema" libConnGetCurrentSchema

setCurrentSchema :: PtrConn -> Text -> IO Bool
setCurrentSchema p s = withCStringLen (T.unpack s) $ \(c,clen) -> isOk <$> libConnSetCurrentSchema p  c (fromIntegral clen)

getEdition :: PtrConn -> IO Text
getEdition = _getConn "edition" libConnGetEdition

getExternalName :: PtrConn -> IO Text
getExternalName = _getConn "externalName" libConnGetExternalName

getInternalName :: PtrConn -> IO Text
getInternalName = _getConn "internalName" libConnGetInternalName

getLTXID :: PtrConn -> IO Text
getLTXID = _getConn "LTXID" libConnGetLTXID

getServerVersion :: PtrConn -> IO (Text, Data_VersionInfo)
getServerVersion = _getConnStrAndObj libConnGetServerVersion "serverVersion"

getObjectType :: PtrConn -> Text -> IO PtrObjectType
getObjectType p name
  = alloca $ \pot ->
    withCStringLen (T.unpack name) $ \(n,nlen) -> do
      ok <- isOk <$> libConnGetObjectType p n (fromIntegral nlen)  pot
      if ok then peek pot else throw $ ConnectionPropNotFound $ "objectType_" <> name

getEncodingInfo :: PtrConn -> IO Data_EncodingInfo
getEncodingInfo p
  = alloca $ \pei -> do
      ok <- isOk <$> libConnGetEncodingInfo p pei
      if ok then peek pei else throw $ ConnectionPropNotFound "encodingInfo"

getStmtCacheSize :: PtrConn -> IO Int
getStmtCacheSize p
  = alloca $ \pc -> do
      ok <- isOk <$> libConnGetStmtCacheSize p pc
      if ok then fromIntegral <$> peek pc else throw $ ConnectionPropNotFound "stmtCacheSize"

-- * ConnectionPool Interace
-- $pool
-- Pool handles are used to represent session pools.
-- They are created using the function 'createPool' and can be closed by calling the function 'closePool'
-- or releasing the last reference to the pool by calling the function 'releasePool'.
-- Pools can be used to create connections by calling the function 'acquiredConnection'.

-- | Acquires a connection from the pool and returns a reference to it. This reference should be released as soon as it is no longer needed.
acquiredConnection :: PtrPool -> IO PtrConn
acquiredConnection
  = _get' (DpiException "acquiredConnection") (\p -> libPoolAcquireConnection p nullPtr 0 nullPtr 0 nullPtr) peek

poolAddRef :: PtrPool -> IO Bool
poolAddRef = runOk libPoolAddRef

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
  = alloca $ \pc   ->
    alloca $ \pcmp ->
    alloca $ \pcop ->
    withCStringLen (T.unpack username) $ \(u,ulen) ->
    withCStringLen (T.unpack password) $ \(p,plen) ->
    withCStringLen (T.unpack connstr)  $ \(c,clen) -> do
      libContextInitCommonCreateParams cxt pcmp
      libContextInitPoolCreateParams   cxt pcop
      peek pcmp >>= poke pcmp . hcmp
      v <- peek pcop
      poke pcop $ hpcp v {getMode = ModePoolGetWait}
      ok <- isOk <$> libPoolCreate cxt u (fromIntegral ulen) p (fromIntegral plen) c (fromIntegral clen) pcmp pcop pc
      if ok then peek pc else throwContextError cxt

-- | Closes the pool and makes it unusable for further activity.
closePool :: PtrPool -> PoolCloseMode -> IO Bool
closePool p mode = isOk <$> libPoolClose p (fe mode)

-- | Releases a reference to the pool. A count of the references to the pool is maintained
-- and when this count reaches zero, the memory associated with the pool is freed
-- and the session pool is closed if that has not already taken place using the function 'closePool'.
releasePool :: PtrPool -> IO Bool
releasePool = runOk libPoolRelease

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
getPoolBusyCount = _get' (DpiException "getPoolBusyCount") libPoolGetBusyCount peekInt

getPoolEncodingInfo :: PtrPool -> IO Data_EncodingInfo
getPoolEncodingInfo = _get' (DpiException "getPoolEncodingInfo") libPoolGetEncodingInfo peek

getPoolMode :: PtrPool -> IO PoolGetMode
getPoolMode = _get' (DpiException "getPoolMode") libPoolGetGetMode (\p -> te <$> peek p)

getPoolMaxLifetimeSession :: PtrPool -> IO Int
getPoolMaxLifetimeSession = _get' (DpiException "getPoolMaxLifetimeSession") libPoolGetMaxLifetimeSession peekInt

getPoolOpenCount :: PtrPool -> IO Int
getPoolOpenCount = _get' (DpiException "libPoolGetOpenCount") libPoolGetOpenCount peekInt

getPoolStmtCacheSize :: PtrPool -> IO Int
getPoolStmtCacheSize = _get' (DpiException "getPoolStmtCacheSize") libPoolGetStmtCacheSize peekInt

getPoolTimeout :: PtrPool -> IO Int
getPoolTimeout = _get' (DpiException "getPoolTimeout") libPoolGetTimeout peekInt

setPoolGetMode :: PtrPool -> PoolGetMode -> IO Bool
setPoolGetMode p mode = isOk <$> libPoolSetGetMode p (fe mode)

setPoolMaxLifetimeSession :: PtrPool -> Int -> IO Bool
setPoolMaxLifetimeSession p maxLifetimeSession = isOk <$> libPoolSetMaxLifetimeSession p (fromIntegral maxLifetimeSession)

setPoolStmtCacheSize :: PtrPool -> Int -> IO Bool
setPoolStmtCacheSize p stmtCacheSize = isOk <$> libPoolSetStmtCacheSize p (fromIntegral stmtCacheSize)

setPoolTimeout :: PtrPool -> Int -> IO Bool
setPoolTimeout p timeout = isOk <$> libPoolSetTimeout p (fromIntegral timeout)


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
createStatement p scrollable sql
  = alloca $ \ps ->
    withCStringLen (T.unpack sql) $ \(s,slen) -> do
      ok <- isOk <$> libConnPrepareStmt p (fromBool scrollable) s (fromIntegral slen) nullPtr 0 ps
      if ok then peek ps else throwDpiException "createStatement"

-- | Normalize SQL, replace newline characters with space characters. and remove semicolon in the end of sql
normalize :: Text -> Text
normalize = T.strip
          . T.dropWhileEnd (==';')
          . T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

-- | Closes the statement and makes it unusable for further work immediately,
-- rather than when the reference count reaches zero.
closeStatement :: PtrStmt -> IO Bool
closeStatement p = isOk <$> libStmtClose p nullPtr 0

-- | Releases a reference to the statement. A count of the references to the statement is maintained
-- and when this count reaches zero, the memory associated with the statement is freed
-- and the statement is closed if that has not already taken place using the function 'closeStatement'.
releaseStatement :: PtrStmt -> IO Bool
releaseStatement = runOk libStmtRelease

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
scrollStatement :: PtrStmt -> FetchMode -> Int -> IO Bool
scrollStatement p mode offset = isOk <$> libStmtScroll p (fe mode) (fromIntegral offset) 0

statementAddRef :: PtrStmt -> IO Bool
statementAddRef p = isOk <$> libStmtAddRef p

-- ** Statement Bind Vars

-- | Binds a variable to a named placeholder in the statement.
-- A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same name.
bindByName :: PtrStmt -> Text -> PtrVar -> IO Bool
bindByName p name var
  = withCStringLen (T.unpack name) $ \(n,nlen) -> isOk <$> libStmtBindByName p n (fromIntegral nlen) var

-- | Binds a variable to a placeholder in the statement by position.
--  A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same position.
bindByPosition :: PtrStmt -> Int -> PtrVar -> IO Bool
bindByPosition p pos var = isOk <$> libStmtBindByPos p (fromIntegral pos) var

-- | Binds a value to a named placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same name.
bindValueByName :: PtrStmt -> Text -> NativeTypeNum -> PtrData -> IO Bool
bindValueByName p name ntn dt
  = withCStringLen (T.unpack name) $ \(n,nlen) -> isOk <$> libStmtBindValueByName p n (fromIntegral nlen) (fe ntn) dt

-- | Binds a value to a placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same position.
bindValueByPosition :: PtrStmt -> Int -> NativeTypeNum -> PtrData -> IO Bool
bindValueByPosition p pos ntn dt = isOk <$> libStmtBindValueByPos p (fromIntegral pos) (fe ntn) dt

-- | Defines the variable that will be used to fetch rows from the statement.
-- A reference to the variable will be retained until the next define is performed on the same position
-- or the statement is closed.
define :: PtrStmt -> Int -> PtrVar -> IO Bool
define p pos var = isOk <$> libStmtDefine p (fromIntegral pos) var

-- | Defines the type of data that will be used to fetch rows from the statement.
-- This is intended for use with the function 'getQueryValue', when the default data type
-- derived from the column metadata needs to be overridden by the application.
-- Internally, a variable is created with the specified data type and size.
defineValue :: PtrStmt -> Int -> OracleTypeNum -> NativeTypeNum -> Int -> Bool -> PtrObjectType -> IO Bool
defineValue p pos otn ntn size isSizeInByte ot = isOk <$> libStmtDefineValue p (fromIntegral pos) (fe otn) (fe ntn) (fromIntegral size) (fromBool isSizeInByte) ot

-- | Returns the number of bind variables in the prepared statement.
-- In SQL statements this is the total number of bind variables whereas in PL/SQL statements
-- this is the count of the unique bind variables.
getBindCount :: PtrStmt -> IO Int
getBindCount = _getStmt "getBindCount" libStmtGetBindCount peekInt

-- | Returns the names of the unique bind variables in the prepared statement.
getBindNames :: PtrStmt -> IO [Text]
getBindNames p = do
  c <- getBindCount p
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
        else throwDpiException "getBindNames"

getStatementInfo :: PtrStmt -> IO Data_StmtInfo
getStatementInfo = _getStmt "getStatementInfo" libStmtGetInfo peek

getFetchArraySize :: PtrStmt -> IO Int
getFetchArraySize = _getStmt "getFetchArraySize" libStmtGetFetchArraySize peekInt

setFetchArraySize :: PtrStmt -> Int -> IO Bool
setFetchArraySize p pos = isOk <$> libStmtSetFetchArraySize p (fromIntegral pos)

getImplicitResult :: PtrStmt -> IO (Maybe PtrStmt)
getImplicitResult = _getStmt "getImplicitResult" libStmtGetImplicitResult (mapM peek . toMaybePtr)

getNumberQueryColumns :: PtrStmt -> IO Int
getNumberQueryColumns = _getStmt "getNumberQueryColumns" libStmtGetNumQueryColumns peekInt

-- | Returns information about the column that is being queried.
getQueryInfo
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO Data_QueryInfo
getQueryInfo p pos = _getStmt "getQueryInfo" (`libStmtGetQueryInfo` fromIntegral pos) peek p

-- | Returns the value of the column at the given position for the currently fetched row,
-- without needing to provide a variable. If the data type of the column needs to be overridden,
-- the function 'defineValue' can be called to specify a different type after executing
-- the statement but before fetching any data.
getQueryValue
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO DataValue
getQueryValue p pos
  = alloca $ \pt ->
    alloca $ \pd -> do
      ok <- isOk <$> libStmtGetQueryValue p (fromIntegral pos) pt pd
      if ok
        then do
          t <- te <$> peek pt
          peek pd >>= _get t
        else throwDpiException "getQueryValue"

-- ** Execute Statement

-- | Executes the statement using the bound values.
-- For queries this makes available metadata which can be acquired using the function 'getQueryInfo'.
-- For non-queries, out and in-out variables are populated with their values.
executeStatement :: PtrStmt -> ExecMode -> IO Int
executeStatement p mode = _getStmt "executeStatement" (`libStmtExecute` fe mode) go p
  where
    go p | nullPtr == p = return 0
         | otherwise    = peekInt p

executeMany :: PtrStmt -> ExecMode -> Int -> IO Bool
executeMany p mode count = isOk <$> libStmtExecuteMany p (fe mode) (fromIntegral count)

-- | Fetches a single row from the statement. If the statement does not refer to a query an error is returned.
--  All columns that have not been defined prior to this call are implicitly defined using the metadata made available when the statement was executed.
fetch :: PtrStmt -> IO (Maybe PageOffset)
fetch p
  = alloca $ \pf ->
    alloca $ \pr -> do
      ok <- isOk <$> libStmtFetch p pf pr
      if ok
        then do
          found <- toBool <$> peek pf
          if found then (Just . fromIntegral) <$> peek pr else return Nothing
        else throwDpiException "fetch"

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
fetchRows p maxRow
  = alloca $ \pri ->
    alloca $ \prf ->
    alloca $ \pmr -> do
      ok <- isOk <$> libStmtFetchRows p (fromIntegral maxRow) pri prf pmr
      if ok
        then do
          index <- peek pri
          num   <- peek prf
          vs    <- fetch p (fromIntegral index) (fromIntegral num)
          more  <- toBool <$> peek pmr
          return (more, vs)
        else throwDpiException "fetchRows"
    where
      fetch st offset limit = do
        count <- getRowCount p
        mapM (getQueryValue p) [1..count]

-- | Returns the number of rows affected by the last DML statement that was executed
-- or the number of rows currently fetched from a query. In all other cases 0 is returned.
getRowCount :: PtrStmt -> IO Int
getRowCount = _getStmt "getRowCount" libStmtGetRowCount peekInt


-- | Returns an array of row counts affected by the last invocation of 'executeMany'
-- with the array DML rowcounts mode enabled.
-- This feature is only available if both client and server are at 12.1.
getRowCounts :: PtrStmt -> IO [Int]
getRowCounts p
  = alloca $ \pc  ->
    alloca $ \pac -> do
      ok <- isOk <$> libStmtGetRowCounts p pc pac
      if ok
        then do
          c   <- peek pc
          pcs <- peekArray (fromIntegral c) pac
          mapM peekInt pcs
        else throwDpiException "getRowCounts"

getSubscrQueryId :: PtrStmt -> IO Word64
getSubscrQueryId = _getStmt "getSubscrQueryId" libStmtGetSubscrQueryId peekInt

getBatchErrorCount :: PtrStmt -> IO Int
getBatchErrorCount = _getStmt "getBatchErrorCount" libStmtGetBatchErrorCount peekInt

getBatchErrors :: PtrStmt -> IO [Data_ErrorInfo]
getBatchErrors p = do
  c <- getBatchErrorCount p
  if c <= 0
    then return []
    else
      allocaArray c $ \par -> do
        ok <- isOk <$> libStmtGetBatchErrors p (fromIntegral c) par
        if ok then peekArray c par else throwDpiException "getBatchErrors"

-- * Lob Interface

newTempLob :: PtrConn -> OracleTypeNum -> IO PtrLob
newTempLob p otn = _get' (DpiException "newTempLob") (flip libConnNewTempLob $ fe otn) peek p

closeLob :: PtrLob -> IO Bool
closeLob = runOk libLobClose

closeLobResource :: PtrLob -> IO Bool
closeLobResource = runOk libLobCloseResource

copyLob :: PtrLob -> IO PtrLob
copyLob = _get' (DpiException "copyLob") libLobCopy peek

flushLob :: PtrLob -> IO Bool
flushLob = runOk libLobFlushBuffer

getLobBufferSize :: PtrLob -> Word64 -> IO Word64
getLobBufferSize p size = _get' (DpiException "libLobGetBufferSize") (`libLobGetBufferSize` fromIntegral size) peekInt p

getLobChunkSize :: PtrLob -> IO Int64
getLobChunkSize = _get' (DpiException "getLobChunkSize") libLobGetChunkSize peekInt

getLobDirectoryAndFileName :: PtrLob -> IO (FilePath, String)
getLobDirectoryAndFileName p
  = alloca $ \pd    ->
    alloca $ \pdlen ->
    alloca $ \pn    ->
    alloca $ \pnlen -> do
      ok <- isOk <$> libLobGetDirectoryAndFileName p pd pdlen pn pnlen
      if ok
        then do
          d    <- peek pd
          dlen <- peek pdlen
          n    <- peek pn
          nlen <- peek pnlen
          fp   <- ts d dlen
          name <- ts n nlen
          return (T.unpack fp, T.unpack name)
        else throwDpiException "getLobDirectoryAndFileName"

setLobDirectoryAndFileName :: PtrLob -> (FilePath, String) -> IO Bool
setLobDirectoryAndFileName p (fp, name)
  = withCStringLen fp   $ \(f, flen) ->
    withCStringLen name $ \(n, nlen) ->
      isOk <$> libLobSetDirectoryAndFileName p f (fromIntegral flen) n (fromIntegral nlen)

lobFileExists :: PtrLob -> IO Bool
lobFileExists = _get' (DpiException "lobFileExists") libLobGetFileExists peekBool

isLobResourceOpen :: PtrLob -> IO Bool
isLobResourceOpen = _get' (DpiException "isLobResourceOpen") libLobGetIsResourceOpen peekBool

getLobSize :: PtrLob -> IO Int64
getLobSize = _get' (DpiException "getLobSize") libLobGetSize peekInt

openLobResource :: PtrLob -> IO Bool
openLobResource p = isOk <$> libLobOpenResource p

releaseLob :: PtrLob -> IO Bool
releaseLob = runOk libLobRelease

trimLob :: PtrLob -> Int64 -> IO Bool
trimLob p size = isOk <$> libLobTrim p (fromIntegral size)

setLobFromBytes :: PtrLob -> Text -> IO Bool
setLobFromBytes p buff
  = withCStringLen (T.unpack buff) $ \(b,blen) ->
      isOk <$> libLobSetFromBytes p b (fromIntegral blen)

type BufferSize = Int64

readLobBytes :: PtrLob -> Page -> BufferSize -> IO Text
readLobBytes p (offset, num) bufferSize
  = alloca $ \pb    ->
    alloca $ \pblen -> do
      poke pblen (fromIntegral bufferSize)
      ok <- isOk <$> libLobReadBytes p (fromIntegral offset) (fromIntegral num) pb pblen
      if ok
        then do
          blen <- peek pblen
          T.pack <$> peekCStringLen (pb, fromIntegral blen)
        else throwDpiException "readLobBytes"

writeLobBytes :: PtrLob -> PageOffset -> Text -> IO Bool
writeLobBytes p size buff
  = withCStringLen (T.unpack buff) $ \(b,blen) ->
      isOk <$> libLobWriteBytes p (fromIntegral size) b (fromIntegral blen)

-- * Object Interface

objectAppendElement :: PtrObject -> NativeTypeNum -> PtrData -> IO Bool
objectAppendElement p ntn pd = isOk <$> libObjectAppendElement p (fe ntn) pd

copyObject :: PtrObject -> IO PtrObject
copyObject = _get' (DpiException "copyObject") libObjectCopy peek

releaseObject :: PtrObject -> IO Bool
releaseObject = runOk libObjectRelease

trimObject :: PtrObject -> Int -> IO Bool
trimObject p c = isOk <$> libObjectTrim p (fromIntegral c)

objectDeleteElementByIndex :: PtrObject -> Int -> IO Bool
objectDeleteElementByIndex p i = isOk <$> libObjectDeleteElementByIndex p (fromIntegral i)

objectSetAttributeValue :: PtrObject -> PtrObjectAttr -> DataValue -> IO Bool
objectSetAttributeValue p pos v = do
  (ntn, pd) <- newData v
  isOk <$> libObjectSetAttributeValue p pos (fe ntn) pd

objectGetAttributeValue :: PtrObject -> PtrObjectAttr -> NativeTypeNum -> IO DataValue
objectGetAttributeValue p poa ntn
  = alloca $ \pd -> do
      ok <- isOk <$> libObjectGetAttributeValue p poa (fe ntn) pd
      if ok then _get ntn pd else throwDpiException "objectGetAttributeValue"

objectGetElementExistsByIndex :: PtrObject -> Int -> IO Bool
objectGetElementExistsByIndex p ind
  = alloca $ \pd -> do
      ok <- isOk <$> libObjectGetElementExistsByIndex p (fromIntegral ind) pd
      if ok then peekBool pd else throwDpiException "objectGetElementExistsByIndex"

objectSetElementValueByIndex :: PtrObject -> Int -> DataValue -> IO Bool
objectSetElementValueByIndex p ind v = do
  (ntn, pd) <- newData v
  isOk <$> libObjectSetElementValueByIndex p (fromIntegral ind) (fe ntn) pd

objectGetElementValueByIndex :: PtrObject -> Int -> NativeTypeNum -> IO DataValue
objectGetElementValueByIndex p pos ntn
  = alloca $ \pd -> do
      ok <- isOk <$> libObjectGetElementValueByIndex p (fromIntegral pos) (fe ntn) pd
      if ok then _get ntn pd else throwDpiException "objectGetElementValueByIndex"

objectGetFirstIndex :: PtrObject -> IO (Maybe Int)
objectGetFirstIndex = _objIndex libObjectGetFirstIndex

objectGetLastIndex :: PtrObject -> IO (Maybe Int)
objectGetLastIndex = _objIndex libObjectGetLastIndex

objectGetNextIndex :: Int -> PtrObject -> IO (Maybe Int)
objectGetNextIndex ind = _objIndex (flip libObjectGetNextIndex $ fromIntegral ind)

objectGetPrevIndex :: Int -> PtrObject -> IO (Maybe Int)
objectGetPrevIndex ind = _objIndex (flip libObjectGetPrevIndex $ fromIntegral ind)

getObjectSize :: PtrObject -> IO Int
getObjectSize = _get' (DpiException "getObjectSize") libObjectGetSize peekInt

getObjectAttrInfo :: PtrObjectAttr -> IO Data_ObjectAttrInfo
getObjectAttrInfo = _get' (DpiException "getObjectAttrInfo") libObjectAttrGetInfo peek

objectAttrAddRef :: PtrObjectAttr -> IO Bool
objectAttrAddRef = runOk libObjectAttrAddRef

releaseObjectAttr :: PtrObjectAttr -> IO Bool
releaseObjectAttr = runOk libObjectAttrRelease

objectTypeAddRef :: PtrObjectType-> IO Bool
objectTypeAddRef = runOk libObjectTypeAddRef

createObjectByType :: PtrObjectType -> IO PtrObject
createObjectByType = _get' (DpiException "createObjectByType") libObjectTypeCreateObject peek

objectTypeGetAttributes :: PtrObjectType -> Int -> IO PtrObjectAttr
objectTypeGetAttributes p num = _get' (DpiException "objectTypeGetAttributes") (flip libObjectTypeGetAttributes $ fromIntegral num) peek p

objectTypeGetInfo :: PtrObjectType -> IO Data_ObjectTypeInfo
objectTypeGetInfo = _get' (DpiException "objectTypeGetInfo") libObjectTypeGetInfo peek

releaseObjectType :: PtrObjectType -> IO Bool
releaseObjectType = runOk libObjectTypeRelease

-- * Rowid Interface

rowidAddRef :: PtrRowid -> IO Bool
rowidAddRef = runOk libRowidAddRef

releaseRowid :: PtrRowid -> IO Bool
releaseRowid = runOk libRowidRelease

rowidGetStringValue :: PtrRowid -> IO Text
rowidGetStringValue p
  = alloca $ \ps    ->
    alloca $ \pslen -> do
      ok <- isOk <$> libRowidGetStringValue p ps pslen
      if ok then join $ ts <$> peek ps <*> peek pslen else throwDpiException "rowidGetStringValue"

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
newVar p otn ntn maxArraySize size sizeIsBytes isArray oto
  = alloca $ \pv ->
    alloca $ \pd -> do
      ok <- isOk <$> libConnNewVar p (fe otn) (fe ntn) (fromIntegral maxArraySize) (fromIntegral size) (fromBool sizeIsBytes) (fromBool isArray) oto pv pd
      if ok then do
          v <- peek pv
          d <- peekArray (fromIntegral maxArraySize) pd
          return (v, d)
        else throwDpiException "newVar"


varAddRef :: PtrVar -> IO Bool
varAddRef = runOk libVarAddRef

copyVar :: PtrVar -> Int -> PtrVar -> Int -> IO Bool
copyVar to toPos from fromPos = isOk <$> libVarCopyData to (fromIntegral toPos) from (fromIntegral fromPos)

varGetData :: PtrVar -> IO [Data]
varGetData p
  = alloca $ \pn ->
    alloca $ \pd -> do
      ok <- isOk <$> libVarGetData p pn pd
      if ok
        then do
          n <- peek pn
          d <- peek pd
          peekArray (fromIntegral n) d
        else throwDpiException "varGetData"

varGetNumberOfElements :: PtrVar -> IO Int
varGetNumberOfElements = _get' (DpiException "varGetNumberOfElements") libVarGetNumElementsInArray peekInt

varGetSizeInBytes :: PtrVar -> IO Int
varGetSizeInBytes = _get' (DpiException "varGetSizeInBytes") libVarGetSizeInBytes peekInt

releaseVar :: PtrVar -> IO Bool
releaseVar = runOk libVarRelease

setVarFromBytes :: PtrVar -> Int -> Text -> IO Bool
setVarFromBytes p pos bytes
  = withCStringLen (T.unpack bytes) $ \(b,blen) -> isOk <$> libVarSetFromBytes p (fromIntegral pos) b (fromIntegral blen)

setVarFromLob :: PtrVar -> Int -> PtrLob -> IO Bool
setVarFromLob p pos lob = isOk <$> libVarSetFromLob p (fromIntegral pos) lob

setVarFromObject :: PtrVar -> Int -> PtrObject -> IO Bool
setVarFromObject p pos obj = isOk <$> libVarSetFromObject p (fromIntegral pos) obj

setVarFromRowid :: PtrVar -> Int -> PtrRowid -> IO Bool
setVarFromRowid p pos obj = isOk <$> libVarSetFromRowid p (fromIntegral pos) obj

setVarFromStatement :: PtrVar -> Int -> PtrStmt -> IO Bool
setVarFromStatement p pos obj = isOk <$> libVarSetFromStmt p (fromIntegral pos) obj

setVarNumberOfElements :: PtrVar -> Int -> IO Bool
setVarNumberOfElements p num = isOk <$> libVarSetNumElementsInArray p (fromIntegral num)

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

import           Control.Exception
import           Data.ByteString.Char8 (pack, unpack)

-- * Context Interface

-- | Create Context
createContext :: IO PtrContext
createContext
  = alloca $ \pc ->
    alloca $ \pe -> do
      ok <- isOk <$> libContextCreate majorVersion minorVersion pc pe
      if ok
        then peek pc
        else peek pe >>= throw . ErrorInfoException

-- | Destroy Context
destroyContext :: PtrContext -> IO Bool
destroyContext p = isOk <$> libContextDestroy p

-- | With Context, 'PtrContext' will be destroyed after run
withContext :: (PtrContext -> IO a) -> IO a
withContext = bracket createContext destroyContext

-- ** Information from Context

getClientVersion :: PtrContext -> IO Data_VersionInfo
getClientVersion p
  = alloca $ \pv -> do
      ok <- isOk <$> libContextGetClientVersion p pv
      if ok then peek pv else throw VersionInfoNotFound

getContextError :: PtrContext -> IO Data_ErrorInfo
getContextError p = alloca $ \pe -> libContextGetError p pe >> peek pe

-- * Connection Interface

-- | Create Connection
createConnection :: PtrContext -- ^ Context
                 -> ByteString -- ^ Username
                 -> ByteString -- ^ Password
                 -> ByteString -- ^ Connection String
                 -> (Data_CommonCreateParams -> Data_CommonCreateParams) -- ^ custom 'Data_CommonCreateParams'
                 -> IO PtrConn
createConnection cxt username password connstr hcmp
  = alloca $ \pc   ->
    alloca $ \pcmp ->
    alloca $ \pcop ->
    useAsCStringLen username $ \(u,ulen) ->
    useAsCStringLen password $ \(p,plen) ->
    useAsCStringLen connstr  $ \(c,clen) -> do
      libContextInitCommonCreateParams cxt pcmp
      libContextInitConnCreateParams   cxt pcop
      a <- peek pcmp
      poke pcmp $ hcmp a
      ok <- isOk <$> libConnCreate cxt u (fromIntegral ulen) p (fromIntegral plen) c (fromIntegral clen) pcmp pcop pc
      if ok then peek pc else throw ConnectionCreateFailed

-- | close connection, but not release resource, plese use 'releaseConnection' to release and close connection
closeConnection :: ConnCloseMode -> PtrConn -> IO Bool
closeConnection mode p = isOk <$> libConnClose p (fe mode) nullPtr 0

-- | ping to check connection available
pingConnection :: PtrConn -> IO Bool
pingConnection p = isOk <$> libConnPing p

-- | with connection
withConnection
  :: PtrContext -- ^ Context
  -> ByteString -- ^ Username
  -> ByteString -- ^ Password
  -> ByteString -- ^ Connection String
  -> ByteString -- ^ NLS_LANG encoding
  -> ByteString -- ^ NLS_NCHAR encoding
  -> (PtrConn -> IO a) -- ^ action use connection
  -> IO a
withConnection p username password connstr lang nchar f
  = bracket
      (createConnection p username password connstr (set lang nchar))
      (closeConnection ModeConnCloseDefault)
      f
  where
    set l n v = v { encoding = l, nencoding = n} :: Data_CommonCreateParams

-- ** Transaction Interface 

beginTransaction 
  :: PtrConn    -- ^ Connection
  -> Int64      -- ^ formatId
  -> ByteString -- ^ transactionId
  -> ByteString -- ^ branchId
  -> IO Bool
beginTransaction p formatId transId branchId
  = useAsCStringLen transId  $ \(t,tlen) ->
    useAsCStringLen branchId $ \(b,blen) -> do
      isOk <$> libConnBeginDistribTrans p (fromIntegral formatId) t (fromIntegral tlen) b (fromIntegral blen)

prepareTransaction :: PtrConn -> IO Bool
prepareTransaction p
  = alloca $ \pc -> do
      ok <- isOk <$> libConnPrepareDistribTrans p pc
      if ok then peekBool pc else throw TransactionPrepareFailed

-- | commit
commitConnection :: PtrConn -> IO Bool
commitConnection p = isOk <$> libConnCommit p

-- | release and close connection
releaseConnection :: PtrConn -> IO Bool
releaseConnection p = isOk <$> libConnRelease p

-- | rollback
rollbackConnection :: PtrConn -> IO Bool
rollbackConnection p = isOk <$> libConnRollback p

-- ** Information from Connection

getCurrentSchema :: PtrConn -> IO ByteString
getCurrentSchema = _getConn "currentSchema" libConnGetCurrentSchema

getEdition :: PtrConn -> IO ByteString
getEdition = _getConn "edition" libConnGetEdition

getExternalName :: PtrConn -> IO ByteString
getExternalName = _getConn "externalName" libConnGetExternalName

getInternalName :: PtrConn -> IO ByteString
getInternalName = _getConn "internalName" libConnGetInternalName

getLTXID :: PtrConn -> IO ByteString
getLTXID = _getConn "LTXID" libConnGetLTXID

getServerVersion :: PtrConn -> IO (ByteString, Data_VersionInfo)
getServerVersion = _getConnStrAndObj libConnGetServerVersion "serverVersion"

getObjectType :: PtrConn -> ByteString -> IO PtrObjectType
getObjectType p name
  = alloca $ \pot ->
    useAsCStringLen name $ \(n,nlen) -> do
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

-- * Statement Interface

-- | Create Statement
createStatement 
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ scrollable
  -> ByteString -- ^ SQL String
  -> IO PtrStmt
createStatement p scrollable sql
  = alloca $ \ps ->
    useAsCStringLen sql $ \(s,slen) -> do
      ok <- isOk <$> libConnPrepareStmt p (fromBool scrollable) s (fromIntegral slen) nullPtr 0 ps
      if ok then peek ps else throw StatementCreateFailed

closeStatement :: PtrStmt -> IO Bool
closeStatement p = isOk <$> libStmtClose p nullPtr 0

releaseStatement :: PtrStmt -> IO Bool
releaseStatement p = isOk <$> libStmtRelease p

scrollStatement :: PtrStmt -> FetchMode -> Int -> IO Bool
scrollStatement p mode offset = isOk <$> libStmtScroll p (fe mode) (fromIntegral offset) 0

-- ** Statement Bind Vars

bindByName :: PtrStmt -> ByteString -> PtrVar -> IO Bool
bindByName p name var
  = useAsCStringLen name $ \(n,nlen) -> isOk <$> libStmtBindByName p n (fromIntegral nlen) var

bindByPosition :: PtrStmt -> Int -> PtrVar -> IO Bool
bindByPosition p pos var = isOk <$> libStmtBindByPos p (fromIntegral pos) var

bindValueByName :: PtrStmt -> ByteString -> NativeTypeNum -> PtrData -> IO Bool
bindValueByName p name ntn dt
  = useAsCStringLen name $ \(n,nlen) -> isOk <$> libStmtBindValueByName p n (fromIntegral nlen) (fe ntn) dt

bindValueByPosition :: PtrStmt -> Int -> NativeTypeNum -> PtrData -> IO Bool
bindValueByPosition p pos ntn dt = isOk <$> libStmtBindValueByPos p (fromIntegral pos) (fe ntn) dt

defineVarByPosition :: PtrStmt -> Int -> PtrVar -> IO Bool
defineVarByPosition p pos var = isOk <$> libStmtDefine p (fromIntegral pos) var

defineValue :: PtrStmt -> Int -> OracleTypeNum -> NativeTypeNum -> Int -> Bool -> PtrObjectType -> IO Bool
defineValue p pos otn ntn size isSizeInByte ot = isOk <$> libStmtDefineValue p (fromIntegral pos) (fe otn) (fe ntn) (fromIntegral size) (fromBool isSizeInByte) ot

getBindCount :: PtrStmt -> IO Int
getBindCount = _getStmt libStmtGetBindCount peekInt

getBindNames :: PtrStmt -> IO [ByteString]
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
          mapM (flip ts al) ac
        else throw StatementGetBindFailed

getFetchArraySize :: PtrStmt -> IO Int
getFetchArraySize = _getStmt libStmtGetFetchArraySize peekInt

setFetchArraySize :: PtrStmt -> Int -> IO Bool
setFetchArraySize p pos = isOk <$> libStmtSetFetchArraySize p (fromIntegral pos)

getImplicitResult :: PtrStmt -> IO (Maybe PtrStmt)
getImplicitResult = _getStmt libStmtGetImplicitResult (mapM peek . toMaybePtr)

getInfo :: PtrStmt -> IO Data_StmtInfo
getInfo = _getStmt libStmtGetInfo peek

getNumberQueryColumns :: PtrStmt -> IO Int
getNumberQueryColumns = _getStmt libStmtGetNumQueryColumns peekInt

getQueryInfo :: PtrStmt -> Int -> IO Data_QueryInfo
getQueryInfo p pos = _getStmt (flip libStmtGetQueryInfo (fromIntegral pos)) peek p

getQueryValue :: PtrStmt -> Int -> IO DataValue
getQueryValue p pos
  = alloca $ \pt ->
    alloca $ \pd -> do
      ok <- isOk <$> libStmtGetQueryValue p (fromIntegral pos) pt pd
      if ok
        then do
          t <- te <$> peek pt
          peek pd >>= _get t
        else throw StatementFetchFailed

-- ** Execute Statement

executeStatement :: PtrStmt -> ExecMode -> IO Int
executeStatement p mode = _getStmt (flip libStmtExecute (fe mode)) peekInt p

executeMany :: PtrStmt -> ExecMode -> Int -> IO Bool
executeMany p mode count = isOk <$> libStmtExecuteMany p (fe mode) (fromIntegral count)

fetch :: PtrStmt -> IO (Maybe Int)
fetch p
  = alloca $ \pf ->
    alloca $ \pr -> do
      ok <- isOk <$> libStmtFetch p pf pr
      if ok
        then do
          found <- toBool <$> peek pf
          if found then (Just . fromIntegral) <$> peek pr else return Nothing
        else throw StatementFetchFailed

-- Index, RowNum
type PageOffset = Int64
type PageLimit  = Int64
type Page = (PageOffset, PageLimit)
type FetchRows a = Page -> IO a

fetchRows :: PtrStmt -> Int -> FetchRows a -> IO (Bool, a)
fetchRows p maxRow fetch
  = alloca $ \pri ->
    alloca $ \prf ->
    alloca $ \pmr -> do
      ok <- isOk <$> libStmtFetchRows p (fromIntegral maxRow) pri prf pmr
      if ok
        then do
          index <- peek pri
          num   <- peek prf
          vs    <- fetch (fromIntegral index, fromIntegral num)
          more  <- toBool <$> peek pmr
          return (more, vs)
        else throw StatementFetchRowFailed

getRowCount :: PtrStmt -> IO Word64
getRowCount = _getStmt libStmtGetRowCount peekInt

getRowCounts :: PtrStmt -> IO [Word64]
getRowCounts p
  = alloca $ \pc  ->
    alloca $ \pac -> do
      ok <- isOk <$> libStmtGetRowCounts p pc pac
      if ok
        then do
          c   <- peek pc
          pcs <- peekArray (fromIntegral c) pac
          mapM peekInt pcs
        else throw StatementFetchFailed

getSubscrQueryId :: PtrStmt -> IO Word64
getSubscrQueryId = _getStmt libStmtGetSubscrQueryId peekInt

getBatchErrorCount :: PtrStmt -> IO Int
getBatchErrorCount = _getStmt libStmtGetBatchErrorCount peekInt

getBatchErrors :: PtrStmt -> IO [Data_ErrorInfo]
getBatchErrors p = do
  c <- getBatchErrorCount p
  if c <= 0
    then return []
    else do
      allocaArray c $ \par -> do
        ok <- isOk <$> libStmtGetBatchErrors p (fromIntegral c) par
        if ok then peekArray c par else throw StatementGetBatchErrorFailed

-- * Lob Interface

newTempLob :: PtrConn -> OracleTypeNum -> IO PtrLob
newTempLob p otn = _get' LobOperateFailed (flip libConnNewTempLob $ fe otn) peek p

closeLob :: PtrLob -> IO Bool
closeLob p = isOk <$> libLobClose p

closeLobResource :: PtrLob -> IO Bool
closeLobResource p = isOk <$> libLobCloseResource p

copyLob :: PtrLob -> IO PtrLob
copyLob = _get' LobOperateFailed libLobCopy peek

flushLob :: PtrLob -> IO Bool
flushLob p = isOk <$> libLobFlushBuffer p

getLobBufferSize :: PtrLob -> Word64 -> IO Word64
getLobBufferSize p size = _get' LobOperateFailed (flip libLobGetBufferSize (fromIntegral size)) peekInt p

getLobChunkSize :: PtrLob -> IO Int64
getLobChunkSize = _get' LobOperateFailed libLobGetChunkSize peekInt

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
          return (unpack fp, unpack name)
        else throw LobOperateFailed

setLobDirectoryAndFileName :: PtrLob -> (FilePath, String) -> IO Bool
setLobDirectoryAndFileName p (fp, name)
  = useAsCStringLen (pack fp)   $ \(f, flen) ->
    useAsCStringLen (pack name) $ \(n, nlen) -> do
      isOk <$> libLobSetDirectoryAndFileName p f (fromIntegral flen) n (fromIntegral nlen)

lobFileExists :: PtrLob -> IO Bool
lobFileExists = _get' LobOperateFailed libLobGetFileExists peekBool

isLobResourceOpen :: PtrLob -> IO Bool
isLobResourceOpen = _get' LobOperateFailed libLobGetIsResourceOpen peekBool

getLobSize :: PtrLob -> IO Int64
getLobSize = _get' LobOperateFailed libLobGetSize peekInt

openLobResource :: PtrLob -> IO Bool
openLobResource p = isOk <$> libLobOpenResource p

releaseLob :: PtrLob -> IO Bool
releaseLob p = isOk <$> libLobRelease p

trimLob :: PtrLob -> Int64 -> IO Bool
trimLob p size = isOk <$> libLobTrim p (fromIntegral size)

setLobFromBytes :: PtrLob -> ByteString -> IO Bool
setLobFromBytes p buff
  = useAsCStringLen buff $ \(b,blen) -> do
      isOk <$> libLobSetFromBytes p b (fromIntegral blen)

type BufferSize = Int64

readLobBytes :: PtrLob -> Page -> BufferSize -> IO ByteString
readLobBytes p (offset, num) bufferSize
  = alloca $ \pb    ->
    alloca $ \pblen -> do
      poke pblen (fromIntegral bufferSize)
      ok <- isOk <$> libLobReadBytes p (fromIntegral offset) (fromIntegral num) pb pblen
      if ok
        then do
          blen <- peek pblen
          packCStringLen (pb, fromIntegral blen)
        else throw LobOperateFailed

writeLobBytes :: PtrLob -> PageOffset -> ByteString -> IO Bool
writeLobBytes p size buff
  = useAsCStringLen buff $ \(b,blen) ->
      isOk <$> libLobWriteBytes p (fromIntegral size) b (fromIntegral blen)

-- * Object Interface

objectAppendElement :: PtrObject -> NativeTypeNum -> PtrData -> IO Bool
objectAppendElement p ntn pd = isOk <$> libObjectAppendElement p (fe ntn) pd

copyObject :: PtrObject -> IO PtrObject
copyObject = _get' LobOperateFailed libObjectCopy peek

releaseObject :: PtrObject -> IO Bool
releaseObject p = isOk <$> libObjectRelease p

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
      if ok then _get ntn pd else throw ObjectOperateFailed

objectGetElementExistsByIndex :: PtrObject -> Int -> IO Bool
objectGetElementExistsByIndex p ind
  = alloca $ \pd -> do
      ok <- isOk <$> libObjectGetElementExistsByIndex p (fromIntegral ind) pd
      if ok then peekBool pd else throw ObjectOperateFailed

objectSetElementValueByIndex :: PtrObject -> Int -> DataValue -> IO Bool
objectSetElementValueByIndex p ind v = do
  (ntn, pd) <- newData v
  isOk <$> libObjectSetElementValueByIndex p (fromIntegral ind) (fe ntn) pd

objectGetElementValueByIndex :: PtrObject -> Int -> NativeTypeNum -> IO DataValue
objectGetElementValueByIndex p pos ntn
  = alloca $ \pd -> do
      ok <- isOk <$> libObjectGetElementValueByIndex p (fromIntegral pos) (fe ntn) pd
      if ok then _get ntn pd else throw ObjectOperateFailed

objectGetFirstIndex :: PtrObject -> IO (Maybe Int)
objectGetFirstIndex = _objIndex libObjectGetFirstIndex

objectGetLastIndex :: PtrObject -> IO (Maybe Int)
objectGetLastIndex = _objIndex libObjectGetLastIndex

objectGetNextIndex :: Int -> PtrObject -> IO (Maybe Int)
objectGetNextIndex ind = _objIndex (flip libObjectGetNextIndex $ fromIntegral ind)

objectGetPrevIndex :: Int -> PtrObject -> IO (Maybe Int)
objectGetPrevIndex ind = _objIndex (flip libObjectGetPrevIndex $ fromIntegral ind)

getObjectSize :: PtrObject -> IO Int
getObjectSize = _get' ObjectOperateFailed libObjectGetSize peekInt

getObjectAttrInfo :: PtrObjectAttr -> IO Data_ObjectAttrInfo
getObjectAttrInfo = _get' ObjectOperateFailed libObjectAttrGetInfo peek

objectAttrAddRef :: PtrObjectAttr -> IO Bool
objectAttrAddRef p = isOk <$> libObjectAttrAddRef p

releaseObjectAttr :: PtrObjectAttr -> IO Bool
releaseObjectAttr p = isOk <$> libObjectAttrRelease p

objectTypeAddRef :: PtrObjectType-> IO Bool
objectTypeAddRef p = isOk <$> libObjectTypeAddRef p

createObjectByType :: PtrObjectType -> IO PtrObject
createObjectByType = _get' ObjectOperateFailed libObjectTypeCreateObject peek

objectTypeGetAttributes :: PtrObjectType -> Int -> IO PtrObjectAttr
objectTypeGetAttributes p num = _get' ObjectOperateFailed (flip libObjectTypeGetAttributes $ fromIntegral num) peek p

objectTypeGetInfo :: PtrObjectType -> IO Data_ObjectTypeInfo
objectTypeGetInfo = _get' ObjectOperateFailed libObjectTypeGetInfo peek

releaseObjectType :: PtrObjectType -> IO Bool
releaseObjectType p = isOk <$> libObjectTypeRelease p

-- * Rowid Interface

rowidAddRef :: PtrRowid -> IO Bool
rowidAddRef p = isOk <$> libRowidAddRef p

releaseRowid :: PtrRowid -> IO Bool
releaseRowid p = isOk <$> libRowidRelease p

rowidGetStringValue :: PtrRowid -> IO ByteString
rowidGetStringValue p
  = alloca $ \ps    ->
    alloca $ \pslen -> do
      ok <- isOk <$> libRowidGetStringValue p ps pslen
      if ok then join $ ts <$> peek ps <*> peek pslen else throw ObjectOperateFailed

-- * Var Interface

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
        else throw ObjectOperateFailed


varAddRef :: PtrVar -> IO Bool
varAddRef p = isOk <$> libVarAddRef p

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
        else throw ObjectOperateFailed

varGetNumberOfElements :: PtrVar -> IO Int
varGetNumberOfElements = _get' ObjectOperateFailed libVarGetNumElementsInArray peekInt

varGetSizeInBytes :: PtrVar -> IO Int
varGetSizeInBytes = _get' ObjectOperateFailed libVarGetSizeInBytes peekInt

releaseVar :: PtrVar -> IO Bool
releaseVar p = isOk <$> libVarRelease p

setVarFromBytes :: PtrVar -> Int -> ByteString -> IO Bool
setVarFromBytes p pos bytes
  = useAsCStringLen bytes $ \(b,blen) -> isOk <$> libVarSetFromBytes p (fromIntegral pos) b (fromIntegral blen)

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

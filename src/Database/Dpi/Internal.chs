module Database.Dpi.Internal where

import           Database.Dpi.Prelude
import           Data.Scientific
import qualified Data.ByteString.Unsafe as B

#include <dpi.h>

{#context prefix="dpi" #}

{-# INLINE majorVersion #-}
majorVersion :: CUInt
majorVersion = {#const DPI_MAJOR_VERSION #}

{-# INLINE minorVersion #-}
minorVersion :: CUInt
minorVersion = {#const DPI_MINOR_VERSION #}

{-# INLINE success #-}
success :: CInt
success = {#const DPI_SUCCESS #}

{-# INLINE defaultDriverName #-}
defaultDriverName :: ByteString
defaultDriverName = {#const DPI_DEFAULT_DRIVER_NAME #}

data Auth

# if DPI_MAJOR_VERSION >= 3
data AuthMode
  = ModeAuthDefault | ModeAuthSysdba | ModeAuthSysoper
  | ModeAuthPrelim | ModeAuthSysasm | ModeAuthSysBkp
  | ModeAuthSysDgd | ModeAuthSyskmt | ModeAuthSysrac
  deriving (Eq, Show)

instance Enum AuthMode where
  toEnum {#const DPI_MODE_AUTH_DEFAULT #} = ModeAuthDefault
  toEnum {#const DPI_MODE_AUTH_SYSDBA #} = ModeAuthSysdba
  toEnum {#const DPI_MODE_AUTH_SYSOPER #} = ModeAuthSysoper
  toEnum {#const DPI_MODE_AUTH_PRELIM #} = ModeAuthPrelim
  toEnum {#const DPI_MODE_AUTH_SYSASM #} = ModeAuthSysasm
  toEnum {#const DPI_MODE_AUTH_SYSBKP #} = ModeAuthSysBkp
  toEnum {#const DPI_MODE_AUTH_SYSDGD #} = ModeAuthSysDgd
  toEnum {#const DPI_MODE_AUTH_SYSKMT #} = ModeAuthSyskmt
  toEnum {#const DPI_MODE_AUTH_SYSRAC #} = ModeAuthSysrac
  toEnum _ = error "Value invalid"
  fromEnum ModeAuthDefault = {#const DPI_MODE_AUTH_DEFAULT #}
  fromEnum ModeAuthSysdba = {#const DPI_MODE_AUTH_SYSDBA #}
  fromEnum ModeAuthSysoper = {#const DPI_MODE_AUTH_SYSOPER #}
  fromEnum ModeAuthPrelim = {#const DPI_MODE_AUTH_PRELIM #}
  fromEnum ModeAuthSysasm = {#const DPI_MODE_AUTH_SYSASM #}
  fromEnum ModeAuthSysBkp = {#const DPI_MODE_AUTH_SYSBKP #}
  fromEnum ModeAuthSysDgd = {#const DPI_MODE_AUTH_SYSDGD #}
  fromEnum ModeAuthSyskmt = {#const DPI_MODE_AUTH_SYSKMT #}
  fromEnum ModeAuthSysrac = {#const DPI_MODE_AUTH_SYSRAC #}
#else
{#enum AuthMode            as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data ConnCloseMode
  = ModeConnCloseDefault | ModeConnCloseDrop | ModeConnCloseRetag
  deriving (Eq, Show)

instance Enum ConnCloseMode where
  toEnum {#const DPI_MODE_CONN_CLOSE_DEFAULT #} = ModeConnCloseDefault
  toEnum {#const DPI_MODE_CONN_CLOSE_DROP #} = ModeConnCloseDrop
  toEnum {#const DPI_MODE_CONN_CLOSE_RETAG #} = ModeConnCloseRetag
  toEnum _ = error "Value invalid"
  fromEnum ModeConnCloseDefault = {#const DPI_MODE_CONN_CLOSE_DEFAULT #}
  fromEnum ModeConnCloseDrop = {#const DPI_MODE_CONN_CLOSE_DROP #}
  fromEnum ModeConnCloseRetag = {#const DPI_MODE_CONN_CLOSE_RETAG #}
#else
{#enum ConnCloseMode       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data CreateMode
  = ModeCreateDefault | ModeCreateThreaded | ModeCreateEvents
  deriving (Eq, Show)

instance Enum CreateMode where
  toEnum {#const DPI_MODE_CREATE_DEFAULT #} = ModeCreateDefault
  toEnum {#const DPI_MODE_CREATE_THREADED #} = ModeCreateThreaded
  toEnum {#const DPI_MODE_CREATE_EVENTS #} = ModeCreateEvents
  toEnum _ = error "Value invalid"
  fromEnum ModeCreateDefault = {#const DPI_MODE_CREATE_DEFAULT #}
  fromEnum ModeCreateThreaded = {#const DPI_MODE_CREATE_THREADED #}
  fromEnum ModeCreateEvents = {#const DPI_MODE_CREATE_EVENTS #}
#else
{#enum CreateMode          as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data DeqMode
  = ModeDeqBrowse | ModeDeqLocked | ModeDeqRemove | ModeDeqRemoveNoData
  deriving(Eq, Show)

instance Enum DeqMode where
  toEnum {#const DPI_MODE_DEQ_BROWSE #} = ModeDeqBrowse
  toEnum {#const DPI_MODE_DEQ_LOCKED #} = ModeDeqLocked
  toEnum {#const DPI_MODE_DEQ_REMOVE #} = ModeDeqRemove
  toEnum {#const DPI_MODE_DEQ_REMOVE_NO_DATA #} = ModeDeqRemoveNoData
  toEnum _ = error "Value invalid"
  fromEnum ModeDeqBrowse = {#const DPI_MODE_DEQ_BROWSE #}
  fromEnum ModeDeqLocked = {#const DPI_MODE_DEQ_LOCKED #}
  fromEnum ModeDeqRemove = {#const DPI_MODE_DEQ_REMOVE #}
  fromEnum ModeDeqRemoveNoData = {#const DPI_MODE_DEQ_REMOVE_NO_DATA #}
#else
{#enum DeqMode             as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data DeqNavigation
  = DeqNavFirstMsg | DeqNavNextTransaction | DeqNavNextMsg
  deriving(Eq, Show)

instance Enum DeqNavigation where
  toEnum {#const DPI_DEQ_NAV_FIRST_MSG #} = DeqNavFirstMsg
  toEnum {#const DPI_DEQ_NAV_NEXT_TRANSACTION #} = DeqNavNextTransaction
  toEnum {#const DPI_DEQ_NAV_NEXT_MSG #} = DeqNavNextMsg
  toEnum _ = error "Value invalid"
  fromEnum DeqNavFirstMsg = {#const DPI_DEQ_NAV_FIRST_MSG #}
  fromEnum DeqNavNextTransaction = {#const DPI_DEQ_NAV_NEXT_TRANSACTION #}
  fromEnum DeqNavNextMsg = {#const DPI_DEQ_NAV_NEXT_MSG #}
#else
{#enum DeqNavigation       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data EventType
  = EventNone | EventStartup | EventShutdown | EventShutdownAny
  | EventDropDb | EventDereg | EventObjchange | EventQuerychange | EventAq
  deriving(Eq, Show)

instance Enum EventType where
  toEnum {#const DPI_EVENT_NONE #} = EventNone
  toEnum {#const DPI_EVENT_STARTUP #} = EventStartup
  toEnum {#const DPI_EVENT_SHUTDOWN #} = EventShutdown
  toEnum {#const DPI_EVENT_SHUTDOWN_ANY #} = EventShutdownAny
  toEnum {#const DPI_EVENT_DROP_DB #} = EventDropDb
  toEnum {#const DPI_EVENT_DEREG #} = EventDereg
  toEnum {#const DPI_EVENT_OBJCHANGE #} = EventObjchange
  toEnum {#const DPI_EVENT_QUERYCHANGE #} = EventQuerychange
  toEnum {#const DPI_EVENT_AQ #} = EventAq
  toEnum _ = error "Value invalid"
  fromEnum EventNone = {#const DPI_EVENT_NONE #}
  fromEnum EventStartup = {#const DPI_EVENT_STARTUP #}
  fromEnum EventShutdown = {#const DPI_EVENT_SHUTDOWN #}
  fromEnum EventShutdownAny = {#const DPI_EVENT_SHUTDOWN_ANY #}
  fromEnum EventDropDb = {#const DPI_EVENT_DROP_DB #}
  fromEnum EventDereg = {#const DPI_EVENT_DEREG #}
  fromEnum EventObjchange = {#const DPI_EVENT_OBJCHANGE #}
  fromEnum EventQuerychange = {#const DPI_EVENT_QUERYCHANGE #}
  fromEnum EventAq = {#const DPI_EVENT_AQ #}
#else
{#enum EventType           as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data ExecMode
  = ModeExecDefault | ModeExecDescribeOnly | ModeExecCommitOnSuccess
  | ModeExecBatchErrors | ModeExecParseOnly | ModeExecArrayDmlRowcounts
  deriving(Eq, Show)

instance Enum ExecMode where
  toEnum {#const DPI_MODE_EXEC_DEFAULT #} = ModeExecDefault
  toEnum {#const DPI_MODE_EXEC_DESCRIBE_ONLY #} = ModeExecDescribeOnly
  toEnum {#const DPI_MODE_EXEC_COMMIT_ON_SUCCESS #} = ModeExecCommitOnSuccess
  toEnum {#const DPI_MODE_EXEC_BATCH_ERRORS #} = ModeExecBatchErrors
  toEnum {#const DPI_MODE_EXEC_PARSE_ONLY #} = ModeExecParseOnly
  toEnum {#const DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS #} = ModeExecArrayDmlRowcounts
  toEnum _ = error "Value invalid"
  fromEnum ModeExecDefault = {#const DPI_MODE_EXEC_DEFAULT #}
  fromEnum ModeExecDescribeOnly = {#const DPI_MODE_EXEC_DESCRIBE_ONLY #}
  fromEnum ModeExecCommitOnSuccess = {#const DPI_MODE_EXEC_COMMIT_ON_SUCCESS #}
  fromEnum ModeExecBatchErrors = {#const DPI_MODE_EXEC_BATCH_ERRORS #}
  fromEnum ModeExecParseOnly = {#const DPI_MODE_EXEC_PARSE_ONLY #}
  fromEnum ModeExecArrayDmlRowcounts = {#const DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS #}
#else
{#enum ExecMode            as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data FetchMode
  = ModeFetchNext | ModeFetchFirst | ModeFetchLast
  | ModeFetchPrior | ModeFetchAbsolute | ModeFetchRelative
  deriving(Eq, Show)

instance Enum FetchMode where
  toEnum {#const DPI_MODE_FETCH_NEXT #} = ModeFetchNext
  toEnum {#const DPI_MODE_FETCH_FIRST #} = ModeFetchFirst
  toEnum {#const DPI_MODE_FETCH_LAST #} = ModeFetchLast
  toEnum {#const DPI_MODE_FETCH_PRIOR #} = ModeFetchPrior
  toEnum {#const DPI_MODE_FETCH_ABSOLUTE #} = ModeFetchAbsolute
  toEnum {#const DPI_MODE_FETCH_RELATIVE #} = ModeFetchRelative
  toEnum _ = error "Value invalid"
  fromEnum ModeFetchNext = {#const DPI_MODE_FETCH_NEXT #}
  fromEnum ModeFetchFirst = {#const DPI_MODE_FETCH_FIRST #}
  fromEnum ModeFetchLast = {#const DPI_MODE_FETCH_LAST #}
  fromEnum ModeFetchPrior = {#const DPI_MODE_FETCH_PRIOR #}
  fromEnum ModeFetchAbsolute = {#const DPI_MODE_FETCH_ABSOLUTE #}
  fromEnum ModeFetchRelative = {#const DPI_MODE_FETCH_RELATIVE #}
#else
{#enum FetchMode           as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data MessageDeliveryMode
  = ModeMsgPersistent | ModeMsgBuffered | ModeMsgPersistentOrBuffered
  deriving(Eq, Show)

instance Enum MessageDeliveryMode where
  toEnum {#const DPI_MODE_MSG_PERSISTENT #} = ModeMsgPersistent
  toEnum {#const DPI_MODE_MSG_BUFFERED #} = ModeMsgBuffered
  toEnum {#const DPI_MODE_MSG_PERSISTENT_OR_BUFFERED #} = ModeMsgPersistentOrBuffered
  toEnum _ = error "Value invalid"
  fromEnum ModeMsgPersistent = {#const DPI_MODE_MSG_PERSISTENT #}
  fromEnum ModeMsgBuffered = {#const DPI_MODE_MSG_BUFFERED #}
  fromEnum ModeMsgPersistentOrBuffered = {#const DPI_MODE_MSG_PERSISTENT_OR_BUFFERED #}
#else
{#enum MessageDeliveryMode as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >=3
data MessageState
  = MsgStateReady | MsgStateWaiting | MsgStateProcessed | MsgStateExpired
  deriving(Eq, Show)

instance Enum MessageState where
  toEnum {#const DPI_MSG_STATE_READY #} = MsgStateReady
  toEnum {#const DPI_MSG_STATE_WAITING #} = MsgStateWaiting
  toEnum {#const DPI_MSG_STATE_PROCESSED #} = MsgStateProcessed
  toEnum {#const DPI_MSG_STATE_EXPIRED #} = MsgStateExpired
  toEnum _ = error "Value invalid"
  fromEnum MsgStateReady = {#const DPI_MSG_STATE_READY #}
  fromEnum MsgStateWaiting = {#const DPI_MSG_STATE_WAITING #}
  fromEnum MsgStateProcessed = {#const DPI_MSG_STATE_PROCESSED #}
  fromEnum MsgStateExpired = {#const DPI_MSG_STATE_EXPIRED #}
#else
{#enum MessageState        as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data NativeTypeNum
  = NativeTypeInt64
  | NativeTypeUint64
  | NativeTypeFloat
  | NativeTypeDouble
  | NativeTypeBytes
  | NativeTypeTimestamp
  | NativeTypeIntervalDs
  | NativeTypeIntervalYm
  | NativeTypeLob
  | NativeTypeObject
  | NativeTypeStmt
  | NativeTypeBoolean
  | NativeTypeRowid
  deriving(Eq, Show)

instance Enum NativeTypeNum where
  toEnum {#const DPI_NATIVE_TYPE_INT64 #} = NativeTypeInt64
  toEnum {#const DPI_NATIVE_TYPE_UINT64 #} = NativeTypeUint64
  toEnum {#const DPI_NATIVE_TYPE_FLOAT #} = NativeTypeFloat
  toEnum {#const DPI_NATIVE_TYPE_DOUBLE #} = NativeTypeDouble
  toEnum {#const DPI_NATIVE_TYPE_BYTES #} = NativeTypeBytes
  toEnum {#const DPI_NATIVE_TYPE_TIMESTAMP #} = NativeTypeTimestamp
  toEnum {#const DPI_NATIVE_TYPE_INTERVAL_DS #} = NativeTypeIntervalDs
  toEnum {#const DPI_NATIVE_TYPE_INTERVAL_YM #} = NativeTypeIntervalYm
  toEnum {#const DPI_NATIVE_TYPE_LOB #} = NativeTypeLob
  toEnum {#const DPI_NATIVE_TYPE_OBJECT #} = NativeTypeObject
  toEnum {#const DPI_NATIVE_TYPE_STMT #} = NativeTypeStmt
  toEnum {#const DPI_NATIVE_TYPE_BOOLEAN #} = NativeTypeBoolean
  toEnum {#const DPI_NATIVE_TYPE_ROWID #} = NativeTypeRowid
  toEnum _ = error "Value invalid"
  fromEnum NativeTypeInt64 = {#const DPI_NATIVE_TYPE_INT64 #}
  fromEnum NativeTypeUint64 = {#const DPI_NATIVE_TYPE_UINT64 #}
  fromEnum NativeTypeFloat = {#const DPI_NATIVE_TYPE_FLOAT #}
  fromEnum NativeTypeDouble = {#const DPI_NATIVE_TYPE_DOUBLE #}
  fromEnum NativeTypeBytes = {#const DPI_NATIVE_TYPE_BYTES #}
  fromEnum NativeTypeTimestamp = {#const DPI_NATIVE_TYPE_TIMESTAMP #}
  fromEnum NativeTypeIntervalDs = {#const DPI_NATIVE_TYPE_INTERVAL_DS #}
  fromEnum NativeTypeIntervalYm = {#const DPI_NATIVE_TYPE_INTERVAL_YM #}
  fromEnum NativeTypeLob = {#const DPI_NATIVE_TYPE_LOB #}
  fromEnum NativeTypeObject = {#const DPI_NATIVE_TYPE_OBJECT #}
  fromEnum NativeTypeStmt = {#const DPI_NATIVE_TYPE_STMT #}
  fromEnum NativeTypeBoolean = {#const DPI_NATIVE_TYPE_BOOLEAN #}
  fromEnum NativeTypeRowid = {#const DPI_NATIVE_TYPE_ROWID #}
#else
{#enum NativeTypeNum       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif


#if DPI_MAJOR_VERSION >= 3
data OpCode
  = OpcodeAllOps
  | OpcodeAllRows
  | OpcodeInsert
  | OpcodeUpdate
  | OpcodeDelete
  | OpcodeAlter
  | OpcodeDrop
  | OpcodeUnknown
  deriving(Eq, Show)

instance Enum OpCode where
  toEnum {#const DPI_OPCODE_ALL_OPS #} = OpcodeAllOps
  toEnum {#const DPI_OPCODE_ALL_ROWS #} = OpcodeAllRows
  toEnum {#const DPI_OPCODE_INSERT #} = OpcodeInsert
  toEnum {#const DPI_OPCODE_UPDATE #} = OpcodeUpdate
  toEnum {#const DPI_OPCODE_DELETE #} = OpcodeDelete
  toEnum {#const DPI_OPCODE_ALTER #} = OpcodeAlter
  toEnum {#const DPI_OPCODE_DROP #} = OpcodeDrop
  toEnum {#const DPI_OPCODE_UNKNOWN #} = OpcodeUnknown
  toEnum _ = error "Value invalid"
  fromEnum OpcodeAllOps = {#const DPI_OPCODE_ALL_OPS #}
  fromEnum OpcodeAllRows = {#const DPI_OPCODE_ALL_ROWS #}
  fromEnum OpcodeInsert = {#const DPI_OPCODE_INSERT #}
  fromEnum OpcodeUpdate = {#const DPI_OPCODE_UPDATE #}
  fromEnum OpcodeDelete = {#const DPI_OPCODE_DELETE #}
  fromEnum OpcodeAlter = {#const DPI_OPCODE_ALTER #}
  fromEnum OpcodeDrop = {#const DPI_OPCODE_DROP #}
  fromEnum OpcodeUnknown = {#const DPI_OPCODE_UNKNOWN #}

#else
{#enum OpCode              as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data OracleTypeNum
  = OracleTypeNone
  | OracleTypeVarchar
  | OracleTypeNvarchar
  | OracleTypeChar
  | OracleTypeNchar
  | OracleTypeRowid
  | OracleTypeRaw
  | OracleTypeNativeFloat
  | OracleTypeNativeDouble
  | OracleTypeNativeInt
  | OracleTypeNumber
  | OracleTypeDate
  | OracleTypeTimestamp
  | OracleTypeTimestampTz
  | OracleTypeTimestampLtz
  | OracleTypeIntervalDs
  | OracleTypeIntervalYm
  | OracleTypeClob
  | OracleTypeNclob
  | OracleTypeBlob
  | OracleTypeBfile
  | OracleTypeStmt
  | OracleTypeBoolean
  | OracleTypeObject
  | OracleTypeLongVarchar
  | OracleTypeLongRaw
  | OracleTypeNativeUint
  deriving(Eq, Show)
  --TODO: OracleTypeMax

instance Enum OracleTypeNum where
  toEnum {#const DPI_ORACLE_TYPE_NONE #} = OracleTypeNone
  toEnum {#const DPI_ORACLE_TYPE_VARCHAR #} = OracleTypeVarchar
  toEnum {#const DPI_ORACLE_TYPE_NVARCHAR #} = OracleTypeNvarchar
  toEnum {#const DPI_ORACLE_TYPE_CHAR #} = OracleTypeChar
  toEnum {#const DPI_ORACLE_TYPE_NCHAR #} = OracleTypeNchar
  toEnum {#const DPI_ORACLE_TYPE_ROWID #} = OracleTypeRowid
  toEnum {#const DPI_ORACLE_TYPE_RAW #} = OracleTypeRaw
  toEnum {#const DPI_ORACLE_TYPE_NATIVE_FLOAT #} = OracleTypeNativeFloat
  toEnum {#const DPI_ORACLE_TYPE_NATIVE_DOUBLE #} = OracleTypeNativeDouble
  toEnum {#const DPI_ORACLE_TYPE_NATIVE_INT #} = OracleTypeNativeInt
  toEnum {#const DPI_ORACLE_TYPE_NUMBER #} = OracleTypeNumber
  toEnum {#const DPI_ORACLE_TYPE_DATE #} = OracleTypeDate
  toEnum {#const DPI_ORACLE_TYPE_TIMESTAMP #} = OracleTypeTimestamp
  toEnum {#const DPI_ORACLE_TYPE_TIMESTAMP_TZ #} = OracleTypeTimestampTz
  toEnum {#const DPI_ORACLE_TYPE_TIMESTAMP_LTZ #} = OracleTypeTimestampLtz
  toEnum {#const DPI_ORACLE_TYPE_INTERVAL_DS #} = OracleTypeIntervalDs
  toEnum {#const DPI_ORACLE_TYPE_INTERVAL_YM #} = OracleTypeIntervalYm
  toEnum {#const DPI_ORACLE_TYPE_CLOB #} = OracleTypeClob
  toEnum {#const DPI_ORACLE_TYPE_NCLOB #} = OracleTypeNclob
  toEnum {#const DPI_ORACLE_TYPE_BLOB #} = OracleTypeBlob
  toEnum {#const DPI_ORACLE_TYPE_BFILE #} = OracleTypeBfile
  toEnum {#const DPI_ORACLE_TYPE_STMT #} = OracleTypeStmt
  toEnum {#const DPI_ORACLE_TYPE_BOOLEAN #} = OracleTypeBoolean
  toEnum {#const DPI_ORACLE_TYPE_OBJECT #} = OracleTypeObject
  toEnum {#const DPI_ORACLE_TYPE_LONG_VARCHAR #} = OracleTypeLongVarchar
  toEnum {#const DPI_ORACLE_TYPE_LONG_RAW #} = OracleTypeLongRaw
  toEnum {#const DPI_ORACLE_TYPE_NATIVE_UINT #} = OracleTypeNativeUint
  toEnum _ = error "Value invalid"
  fromEnum OracleTypeNone = {#const DPI_ORACLE_TYPE_NONE #}
  fromEnum OracleTypeVarchar = {#const DPI_ORACLE_TYPE_VARCHAR #}
  fromEnum OracleTypeNvarchar = {#const DPI_ORACLE_TYPE_NVARCHAR #}
  fromEnum OracleTypeChar = {#const DPI_ORACLE_TYPE_CHAR #}
  fromEnum OracleTypeNchar = {#const DPI_ORACLE_TYPE_NCHAR #}
  fromEnum OracleTypeRowid = {#const DPI_ORACLE_TYPE_ROWID #}
  fromEnum OracleTypeRaw = {#const DPI_ORACLE_TYPE_RAW #}
  fromEnum OracleTypeNativeFloat = {#const DPI_ORACLE_TYPE_NATIVE_FLOAT #}
  fromEnum OracleTypeNativeDouble = {#const DPI_ORACLE_TYPE_NATIVE_DOUBLE #}
  fromEnum OracleTypeNativeInt = {#const DPI_ORACLE_TYPE_NATIVE_INT #}
  fromEnum OracleTypeNumber = {#const DPI_ORACLE_TYPE_NUMBER #}
  fromEnum OracleTypeDate = {#const DPI_ORACLE_TYPE_DATE #}
  fromEnum OracleTypeTimestamp = {#const DPI_ORACLE_TYPE_TIMESTAMP #}
  fromEnum OracleTypeTimestampTz = {#const DPI_ORACLE_TYPE_TIMESTAMP_TZ #}
  fromEnum OracleTypeTimestampLtz = {#const DPI_ORACLE_TYPE_TIMESTAMP_LTZ #}
  fromEnum OracleTypeIntervalDs = {#const DPI_ORACLE_TYPE_INTERVAL_DS #}
  fromEnum OracleTypeIntervalYm = {#const DPI_ORACLE_TYPE_INTERVAL_YM #}
  fromEnum OracleTypeClob = {#const DPI_ORACLE_TYPE_CLOB #}
  fromEnum OracleTypeNclob = {#const DPI_ORACLE_TYPE_NCLOB #}
  fromEnum OracleTypeBlob = {#const DPI_ORACLE_TYPE_BLOB #}
  fromEnum OracleTypeBfile = {#const DPI_ORACLE_TYPE_BFILE #}
  fromEnum OracleTypeStmt = {#const DPI_ORACLE_TYPE_STMT #}
  fromEnum OracleTypeBoolean = {#const DPI_ORACLE_TYPE_BOOLEAN #}
  fromEnum OracleTypeObject = {#const DPI_ORACLE_TYPE_OBJECT #}
  fromEnum OracleTypeLongVarchar = {#const DPI_ORACLE_TYPE_LONG_VARCHAR #}
  fromEnum OracleTypeLongRaw = {#const DPI_ORACLE_TYPE_LONG_RAW #}
  fromEnum OracleTypeNativeUint = {#const DPI_ORACLE_TYPE_NATIVE_UINT #}
#else
{#enum OracleTypeNum       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data PoolCloseMode
  = ModePoolCloseDefault | ModePoolCloseForce
  deriving(Eq, Show)

instance Enum PoolCloseMode where
  toEnum {#const DPI_MODE_POOL_CLOSE_DEFAULT #} = ModePoolCloseDefault
  toEnum {#const DPI_MODE_POOL_CLOSE_FORCE #} = ModePoolCloseForce
  toEnum _ = error "Value invalid"
  fromEnum ModePoolCloseDefault = {#const DPI_MODE_POOL_CLOSE_DEFAULT #}
  fromEnum ModePoolCloseForce = {#const DPI_MODE_POOL_CLOSE_FORCE #}
#else
{#enum PoolCloseMode       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data PoolGetMode
  = ModePoolGetWait | ModePoolGetNowait | ModePoolGetForceget | ModePoolGetTimedwait
  deriving(Eq, Show)

instance Enum PoolGetMode where
  toEnum {#const DPI_MODE_POOL_GET_WAIT #} = ModePoolGetWait
  toEnum {#const DPI_MODE_POOL_GET_NOWAIT #} = ModePoolGetNowait
  toEnum {#const DPI_MODE_POOL_GET_FORCEGET #} = ModePoolGetForceget
  toEnum {#const DPI_MODE_POOL_GET_TIMEDWAIT #} = ModePoolGetTimedwait
  toEnum _ = error "Value invalid"
  fromEnum ModePoolGetWait = {#const DPI_MODE_POOL_GET_WAIT #}
  fromEnum ModePoolGetNowait = {#const DPI_MODE_POOL_GET_NOWAIT #}
  fromEnum ModePoolGetForceget = {#const DPI_MODE_POOL_GET_FORCEGET #}
  fromEnum ModePoolGetTimedwait = {#const DPI_MODE_POOL_GET_TIMEDWAIT #}
#else
{#enum PoolGetMode         as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data Purity
  = PurityDefault | PurityNew | PuritySelf
  deriving(Eq, Show)

instance Enum Purity where
  toEnum {#const DPI_PURITY_DEFAULT #} = PurityDefault
  toEnum {#const DPI_PURITY_NEW #} = PurityNew
  toEnum {#const DPI_PURITY_SELF #} = PuritySelf
  toEnum _ = error "Value invalid"
  fromEnum PurityDefault = {#const DPI_PURITY_DEFAULT #}
  fromEnum PurityNew = {#const DPI_PURITY_NEW #}
  fromEnum PuritySelf = {#const DPI_PURITY_SELF #}
#else
{#enum Purity              as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data ShutdownMode
  = ModeShutdownDefault | ModeShutdownTransactional | ModeShutdownTransactionalLocal
  | ModeShutdownImmediate | ModeShutdownAbort | ModeShutdownFinal
  deriving(Eq, Show)

instance Enum ShutdownMode where
  toEnum {#const DPI_MODE_SHUTDOWN_DEFAULT #} = ModeShutdownDefault
  toEnum {#const DPI_MODE_SHUTDOWN_TRANSACTIONAL #} = ModeShutdownTransactional
  toEnum {#const DPI_MODE_SHUTDOWN_TRANSACTIONAL_LOCAL #} = ModeShutdownTransactionalLocal
  toEnum {#const DPI_MODE_SHUTDOWN_IMMEDIATE #} = ModeShutdownImmediate
  toEnum {#const DPI_MODE_SHUTDOWN_ABORT #} = ModeShutdownAbort
  toEnum {#const DPI_MODE_SHUTDOWN_FINAL #} = ModeShutdownFinal
  toEnum _ = error "Value invalid"
  fromEnum ModeShutdownDefault = {#const DPI_MODE_SHUTDOWN_DEFAULT #}
  fromEnum ModeShutdownTransactional = {#const DPI_MODE_SHUTDOWN_TRANSACTIONAL #}
  fromEnum ModeShutdownTransactionalLocal = {#const DPI_MODE_SHUTDOWN_TRANSACTIONAL_LOCAL #}
  fromEnum ModeShutdownImmediate = {#const DPI_MODE_SHUTDOWN_IMMEDIATE #}
  fromEnum ModeShutdownAbort = {#const DPI_MODE_SHUTDOWN_ABORT #}
  fromEnum ModeShutdownFinal = {#const DPI_MODE_SHUTDOWN_FINAL #}
#else
{#enum ShutdownMode        as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data StartupMode
  = ModeStartupDefault | ModeStartupForce | ModeStartupRestrict
  deriving(Eq, Show)

instance Enum StartupMode where
  toEnum {#const DPI_MODE_STARTUP_DEFAULT #} = ModeStartupDefault
  toEnum {#const DPI_MODE_STARTUP_FORCE #} = ModeStartupForce
  toEnum {#const DPI_MODE_STARTUP_RESTRICT #} = ModeStartupRestrict
  toEnum _ = error "Value invalid"
  fromEnum ModeStartupDefault = {#const DPI_MODE_STARTUP_DEFAULT #}
  fromEnum ModeStartupForce = {#const DPI_MODE_STARTUP_FORCE #}
  fromEnum ModeStartupRestrict = {#const DPI_MODE_STARTUP_RESTRICT #}
#else
{#enum StartupMode         as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data StatementType
  = StmtTypeUnknown | StmtTypeSelect | StmtTypeUpdate | StmtTypeDelete
  | StmtTypeInsert | StmtTypeCreate | StmtTypeDrop | StmtTypeAlter
  | StmtTypeBegin | StmtTypeDeclare | StmtTypeCall | StmtTypeExplainPlan
  | StmtTypeMerge | StmtTypeRollback | StmtTypeCommit
  deriving(Eq, Show)

instance Enum StatementType where
  toEnum {#const DPI_STMT_TYPE_UNKNOWN #} = StmtTypeUnknown
  toEnum {#const DPI_STMT_TYPE_SELECT #} = StmtTypeSelect
  toEnum {#const DPI_STMT_TYPE_UPDATE #} = StmtTypeUpdate
  toEnum {#const DPI_STMT_TYPE_DELETE #} = StmtTypeDelete
  toEnum {#const DPI_STMT_TYPE_INSERT #} = StmtTypeInsert
  toEnum {#const DPI_STMT_TYPE_CREATE #} = StmtTypeCreate
  toEnum {#const DPI_STMT_TYPE_DROP #} = StmtTypeDrop
  toEnum {#const DPI_STMT_TYPE_ALTER #} = StmtTypeAlter
  toEnum {#const DPI_STMT_TYPE_BEGIN #} = StmtTypeBegin
  toEnum {#const DPI_STMT_TYPE_DECLARE #} = StmtTypeDeclare
  toEnum {#const DPI_STMT_TYPE_CALL #} = StmtTypeCall
  toEnum {#const DPI_STMT_TYPE_EXPLAIN_PLAN #} = StmtTypeExplainPlan
  toEnum {#const DPI_STMT_TYPE_MERGE #} = StmtTypeMerge
  toEnum {#const DPI_STMT_TYPE_ROLLBACK #} = StmtTypeRollback
  toEnum {#const DPI_STMT_TYPE_COMMIT #} = StmtTypeCommit
  toEnum _ = error "Value invalid"
  fromEnum StmtTypeUnknown = {#const DPI_STMT_TYPE_UNKNOWN #}
  fromEnum StmtTypeSelect = {#const DPI_STMT_TYPE_SELECT #}
  fromEnum StmtTypeUpdate = {#const DPI_STMT_TYPE_UPDATE #}
  fromEnum StmtTypeDelete = {#const DPI_STMT_TYPE_DELETE #}
  fromEnum StmtTypeInsert = {#const DPI_STMT_TYPE_INSERT #}
  fromEnum StmtTypeCreate = {#const DPI_STMT_TYPE_CREATE #}
  fromEnum StmtTypeDrop = {#const DPI_STMT_TYPE_DROP #}
  fromEnum StmtTypeAlter = {#const DPI_STMT_TYPE_ALTER #}
  fromEnum StmtTypeBegin = {#const DPI_STMT_TYPE_BEGIN #}
  fromEnum StmtTypeDeclare = {#const DPI_STMT_TYPE_DECLARE #}
  fromEnum StmtTypeCall = {#const DPI_STMT_TYPE_CALL #}
  fromEnum StmtTypeExplainPlan = {#const DPI_STMT_TYPE_EXPLAIN_PLAN #}
  fromEnum StmtTypeMerge = {#const DPI_STMT_TYPE_MERGE #}
  fromEnum StmtTypeRollback = {#const DPI_STMT_TYPE_ROLLBACK #}
  fromEnum StmtTypeCommit = {#const DPI_STMT_TYPE_COMMIT #}
#else
{#enum StatementType       as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
data SubscrGroupingClass = SubscrGroupingClassTime deriving (Eq, Show)
instance Enum SubscrGroupingClass where
  toEnum {#const DPI_SUBSCR_GROUPING_CLASS_TIME #} = SubscrGroupingClassTime
  toEnum _ = error "Value invalid"
  fromEnum _ = {#const DPI_SUBSCR_GROUPING_CLASS_TIME #}

data SubscrGroupingType
  = SubscrGroupingTypeSummary
  | SubscrGroupingTypeLast
  deriving (Eq, Show)

instance Enum SubscrGroupingType where
  toEnum {#const DPI_SUBSCR_GROUPING_TYPE_SUMMARY #} = SubscrGroupingTypeSummary
  toEnum {#const DPI_SUBSCR_GROUPING_TYPE_LAST    #} = SubscrGroupingTypeLast
  toEnum _ = error "Value invalid"
  fromEnum SubscrGroupingTypeSummary = {#const DPI_SUBSCR_GROUPING_TYPE_SUMMARY #}
  fromEnum SubscrGroupingTypeLast    = {#const DPI_SUBSCR_GROUPING_TYPE_LAST    #}
#endif

#if DPI_MAJOR_VERSION >= 3
data SubscrNamespace
  = SubscrNamespaceAq | SubscrNamespaceDbchange
  deriving(Eq, Show)

instance Enum SubscrNamespace where
  toEnum {#const DPI_SUBSCR_NAMESPACE_AQ #} = SubscrNamespaceAq
  toEnum {#const DPI_SUBSCR_NAMESPACE_DBCHANGE #} = SubscrNamespaceDbchange
  toEnum _ = error "Value invalid"
  fromEnum SubscrNamespaceAq = {#const DPI_SUBSCR_NAMESPACE_AQ #}
  fromEnum SubscrNamespaceDbchange = {#const DPI_SUBSCR_NAMESPACE_DBCHANGE #}
#else
{#enum SubscrNamespace     as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data SubscrProtocol
  = SubscrProtoCallback | SubscrProtoMail | SubscrProtoPlsql | SubscrProtoHttp
  deriving(Eq, Show)

instance Enum SubscrProtocol where
  toEnum {#const DPI_SUBSCR_PROTO_CALLBACK #} = SubscrProtoCallback
  toEnum {#const DPI_SUBSCR_PROTO_MAIL #} = SubscrProtoMail
  toEnum {#const DPI_SUBSCR_PROTO_PLSQL #} = SubscrProtoPlsql
  toEnum {#const DPI_SUBSCR_PROTO_HTTP #} = SubscrProtoHttp
  toEnum _ = error "Value invalid"
  fromEnum SubscrProtoCallback = {#const DPI_SUBSCR_PROTO_CALLBACK #}
  fromEnum SubscrProtoMail = {#const DPI_SUBSCR_PROTO_MAIL #}
  fromEnum SubscrProtoPlsql = {#const DPI_SUBSCR_PROTO_PLSQL #}
  fromEnum SubscrProtoHttp = {#const DPI_SUBSCR_PROTO_HTTP #}
#else
{#enum SubscrProtocol      as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data SubscrQOS
  = SubscrQosReliable | SubscrQosDeregNfy | SubscrQosRowids
  | SubscrQosQuery | SubscrQosBestEffort
  deriving(Eq, Show)

instance Enum SubscrQOS where
  toEnum {#const DPI_SUBSCR_QOS_RELIABLE #} = SubscrQosReliable
  toEnum {#const DPI_SUBSCR_QOS_DEREG_NFY #} = SubscrQosDeregNfy
  toEnum {#const DPI_SUBSCR_QOS_ROWIDS #} = SubscrQosRowids
  toEnum {#const DPI_SUBSCR_QOS_QUERY #} = SubscrQosQuery
  toEnum {#const DPI_SUBSCR_QOS_BEST_EFFORT #} = SubscrQosBestEffort
  toEnum _ = error "Value invalid"
  fromEnum SubscrQosReliable = {#const DPI_SUBSCR_QOS_RELIABLE #}
  fromEnum SubscrQosDeregNfy = {#const DPI_SUBSCR_QOS_DEREG_NFY #}
  fromEnum SubscrQosRowids = {#const DPI_SUBSCR_QOS_ROWIDS #}
  fromEnum SubscrQosQuery = {#const DPI_SUBSCR_QOS_QUERY #}
  fromEnum SubscrQosBestEffort = {#const DPI_SUBSCR_QOS_BEST_EFFORT #}
#else
{#enum SubscrQOS           as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

#if DPI_MAJOR_VERSION >= 3
data Visibility = VisibilityImmediate | VisibilityOnCommit
                deriving(Eq, Show)

instance Enum Visibility where
  toEnum {#const DPI_VISIBILITY_IMMEDIATE #} = VisibilityImmediate
  toEnum {#const DPI_VISIBILITY_ON_COMMIT #} = VisibilityOnCommit
  toEnum _ = error "Value invalid"
  fromEnum VisibilityImmediate = {#const DPI_VISIBILITY_IMMEDIATE #}
  fromEnum VisibilityOnCommit = {#const DPI_VISIBILITY_ON_COMMIT #}
#else
{#enum Visibility          as ^ {underscoreToCase} deriving (Eq, Show) #}
#endif

-- Handler
{#pointer *Conn       as DPI_Conn       foreign newtype #}
{#pointer *Pool       as DPI_Pool       foreign newtype #}
{#pointer *Stmt       as DPI_Stmt       foreign newtype #}
{#pointer *Var        as DPI_Var        foreign newtype #}
{#pointer *Lob        as DPI_Lob        foreign newtype #}
{#pointer *Object     as DPI_Object     foreign newtype #}
{#pointer *ObjectAttr as DPI_ObjectAttr foreign newtype #}
{#pointer *ObjectType as DPI_ObjectType foreign newtype #}
{#pointer *Rowid      as DPI_Rowid      foreign newtype #}
{#pointer *Subscr     as DPI_Subscr     foreign newtype #}
{#pointer *DeqOptions as DPI_DeqOptions foreign newtype #}
{#pointer *EnqOptions as DPI_EnqOptions foreign newtype #}
{#pointer *MsgProps   as DPI_MsgProps   foreign newtype #}

{#pointer *Context    as DPI_Context    foreign newtype #}

type HasCxtPtr a   = (PtrContext, Ptr a)
type PtrConn       = HasCxtPtr DPI_Conn
type PtrPool       = HasCxtPtr DPI_Pool
type PtrStmt       = HasCxtPtr DPI_Stmt
type PtrVar        = HasCxtPtr DPI_Var
type PtrLob        = HasCxtPtr DPI_Lob
type PtrObject     = HasCxtPtr DPI_Object
type PtrObjectAttr = HasCxtPtr DPI_ObjectAttr
type PtrObjectType = HasCxtPtr DPI_ObjectType
type PtrRowid      = HasCxtPtr DPI_Rowid
type PtrSubscr     = HasCxtPtr DPI_Subscr
type PtrDeqOptions = HasCxtPtr DPI_DeqOptions
type PtrEnqOptions = HasCxtPtr DPI_EnqOptions
type PtrMsgProps   = HasCxtPtr DPI_MsgProps
type PtrContext    = Ptr DPI_Context

--        Inner               Data
{#pointer *Bytes      as PtrBytes      -> Data_Bytes      #}
{#pointer *IntervalDS as PtrIntervalDS -> Data_IntervalDS #}
{#pointer *IntervalYM as PtrIntervalYM -> Data_IntervalYM #}
{#pointer *Timestamp  as PtrTimestamp  -> Data_Timestamp  #}

data Data_Bytes = Data_Bytes
  { bytes    :: !CStringLen
  , encoding :: !ByteString
  } deriving Show

instance Storable Data_Bytes where
  sizeOf    _ = {#sizeof  Bytes#}
  alignment _ = {#alignof Bytes#}
  poke      _ = noImplement
  peek      p = do
    ptr'     <- {#get Bytes -> ptr      #} p
    len      <- {#get Bytes -> length   #} p
    encoding <- {#get Bytes -> encoding #} p >>= ts
    let bytes = (ptr', fromIntegral len)
    return Data_Bytes {..}

data Data_IntervalDS = Data_IntervalDS
  { days     :: !CInt
  , hours    :: !CInt
  , minutes  :: !CInt
  , seconds  :: !CInt
  , fseconds :: !CInt
  } deriving Show

instance Storable Data_IntervalDS where
  sizeOf    _ = {#sizeof  IntervalDS #}
  alignment _ = {#alignof IntervalDS #}
  poke      _ = noImplement
  peek      p = do
    days     <- {#get IntervalDS -> days     #} p
    hours    <- {#get IntervalDS -> hours    #} p
    minutes  <- {#get IntervalDS -> minutes  #} p
    seconds  <- {#get IntervalDS -> seconds  #} p
    fseconds <- {#get IntervalDS -> fseconds #} p
    return Data_IntervalDS {..}

data Data_IntervalYM = Data_IntervalYM
  { years    :: !CInt
  , months   :: !CInt
  } deriving Show

instance Storable Data_IntervalYM where
  sizeOf    _ = {#sizeof  IntervalYM #}
  alignment _ = {#alignof IntervalYM #}
  poke      _ = noImplement
  peek      p = do
    years    <- {#get IntervalYM -> years    #} p
    months   <- {#get IntervalYM -> months   #} p
    return Data_IntervalYM {..}

data Data_Timestamp  = Data_Timestamp
  { year           :: !CShort
  , month          :: !CUChar
  , day            :: !CUChar
  , hour           :: !CUChar
  , minute         :: !CUChar
  , second         :: !CUChar
  , fsecond        :: !CUInt
  , tzHourOffset   :: !CSChar
  , tzMinuteOffset :: !CSChar
  } deriving Show

instance Storable Data_Timestamp where
  sizeOf    _ = {#sizeof  Timestamp #}
  alignment _ = {#alignof Timestamp #}
  poke      _ = noImplement
  peek      p = do
    year           <- {#get Timestamp -> year           #} p
    month          <- {#get Timestamp -> month          #} p
    day            <- {#get Timestamp -> day            #} p
    hour           <- {#get Timestamp -> hour           #} p
    minute         <- {#get Timestamp -> minute         #} p
    second         <- {#get Timestamp -> second         #} p
    fsecond        <- {#get Timestamp -> fsecond        #} p
    tzHourOffset   <- {#get Timestamp -> tzHourOffset   #} p
    tzMinuteOffset <- {#get Timestamp -> tzMinuteOffset #} p
    return Data_Timestamp {..}

--        Data
{#pointer *AppContext         as PtrAppContext         -> Data_AppContext         #}
{#pointer *CommonCreateParams as PtrCommonCreateParams -> Data_CommonCreateParams #}
{#pointer *ConnCreateParams   as PtrConnCreateParams   -> Data_ConnCreateParams   #}
{#pointer *Data               as PtrData               -> Data                    #}
{#pointer *DataTypeInfo       as PtrDataTypeInfo       -> Data_DataTypeInfo       #}
{#pointer *EncodingInfo       as PtrEncodingInfo       -> Data_EncodingInfo       #}
{#pointer *ErrorInfo          as PtrErrorInfo          -> Data_ErrorInfo          #}
{#pointer *ObjectAttrInfo     as PtrObjectAttrInfo     -> Data_ObjectAttrInfo     #}
{#pointer *ObjectTypeInfo     as PtrObjectTypeInfo     -> Data_ObjectTypeInfo     #}
{#pointer *PoolCreateParams   as PtrPoolCreateParams   -> Data_PoolCreateParams   #}
{#pointer *QueryInfo          as PtrQueryInfo          -> Data_QueryInfo          #}
{#pointer *ShardingKeyColumn  as PtrShardingKeyColumn  -> Data_ShardingKeyColumn  #}
{#pointer *StmtInfo           as PtrStmtInfo           -> Data_StmtInfo           #}
{#pointer *SubscrCreateParams as PtrSubscrCreateParams -> Data_SubscrCreateParams #}
{#pointer *SubscrMessage      as PtrSubscrMessage      -> Data_SubscrMessage      #}
{#pointer *SubscrMessageQuery as PtrSubscrMessageQuery -> Data_SubscrMessageQuery #}
{#pointer *SubscrMessageRow   as PtrSubscrMessageRow   -> Data_SubscrMessageRow   #}
{#pointer *SubscrMessageTable as PtrSubscrMessageTable -> Data_SubscrMessageTable #}
{#pointer *VersionInfo        as PtrVersionInfo        -> Data_VersionInfo        #}

data Data_AppContext  = Data_AppContext
  { namespaceName       :: !CStringLen
  , name                :: !CStringLen
  , value               :: !CStringLen
  } deriving Show

instance Storable Data_AppContext where
  sizeOf    _ = {#sizeof  AppContext #}
  alignment _ = {#alignof AppContext #}
  poke      p Data_AppContext{..} = do
    let (n,nlen) = namespaceName
        (m,mlen) = name
        (v,vlen) = value
    {#set AppContext -> namespaceName       #} p n
    {#set AppContext -> namespaceNameLength #} p (fromIntegral nlen)
    {#set AppContext -> name                #} p m
    {#set AppContext -> nameLength          #} p (fromIntegral mlen)
    {#set AppContext -> value               #} p v
    {#set AppContext -> valueLength         #} p (fromIntegral vlen)
  peek      p = do
    namespaceName'      <- {#get AppContext -> namespaceName       #} p
    namespaceNameLength <- {#get AppContext -> namespaceNameLength #} p
    name'               <- {#get AppContext -> name                #} p
    nameLength          <- {#get AppContext -> nameLength          #} p
    value'              <- {#get AppContext -> value               #} p
    valueLength         <- {#get AppContext -> valueLength         #} p
    let namespaceName = (namespaceName', fromIntegral namespaceNameLength)
        name          = (name',          fromIntegral nameLength)
        value         = (value',         fromIntegral valueLength)
    return Data_AppContext {..}

data Data_CommonCreateParams  = Data_CommonCreateParams
  { createMode       :: !CreateMode
  , encoding         :: !ByteString
  , nencoding        :: !ByteString
  , edition          :: !CStringLen
  , driverName       :: !CStringLen
  } deriving Show

instance Storable Data_CommonCreateParams where
  sizeOf    _ = {#sizeof  CommonCreateParams #}
  alignment _ = {#alignof CommonCreateParams #}
  poke    p Data_CommonCreateParams{..} = B.unsafeUseAsCString encoding $ \pe -> B.unsafeUseAsCString nencoding $ \pn -> do
    let (e,elen) = edition
        (d,dlen) = driverName
    {#set CommonCreateParams -> createMode       #} p (fe createMode)
    {#set CommonCreateParams -> encoding         #} p pe
    {#set CommonCreateParams -> nencoding        #} p pn
    {#set CommonCreateParams -> edition          #} p e
    {#set CommonCreateParams -> editionLength    #} p (fromIntegral elen)
    {#set CommonCreateParams -> driverName       #} p d
    {#set CommonCreateParams -> driverNameLength #} p (fromIntegral dlen)
  peek      p = do
    createMode       <- te <$> {#get CommonCreateParams -> createMode       #} p
    encoding         <-        {#get CommonCreateParams -> encoding         #} p >>= ts
    nencoding        <-        {#get CommonCreateParams -> nencoding        #} p >>= ts
    edition'         <-        {#get CommonCreateParams -> edition          #} p
    editionLength    <-        {#get CommonCreateParams -> editionLength    #} p
    driverName'      <-        {#get CommonCreateParams -> driverName       #} p
    driverNameLength <-        {#get CommonCreateParams -> driverNameLength #} p
    let edition    = (edition',    fromIntegral editionLength)
        driverName = (driverName', fromIntegral driverNameLength)
    return Data_CommonCreateParams {..}

data Data_ConnCreateParams  = Data_ConnCreateParams
  { authMode                   :: !AuthMode
  , connectionClass            :: !CStringLen
  , purity                     :: !Purity
  , newPassword                :: !CStringLen
  , appContext                 :: !PtrAppContext
  , numAppContext              :: !CUInt
  , externalAuth               :: !CInt
  , externalHandle             :: !(Ptr ())
  , pool                       :: !(Ptr DPI_Pool)
  , tag                        :: !CStringLen
  , matchAnyTag                :: !CInt
  , outTag                     :: !CStringLen
  , outTagFound                :: !CInt
  , shardingKeyColumns         :: !PtrShardingKeyColumn
  , numShardingKeyColumns      :: !CUChar
  , superShardingKeyColumns    :: !PtrShardingKeyColumn
  , numSuperShardingKeyColumns :: !CUChar
  } deriving Show

instance Storable Data_ConnCreateParams where
  sizeOf    _ = {#sizeof  ConnCreateParams #}
  alignment _ = {#alignof ConnCreateParams #}
  poke    p Data_ConnCreateParams{..} = do
    let (cc,cclen) = connectionClass
        (np,nplen) = newPassword
        (tg,tglen) = tag
        (og,oglen) = outTag
    {#set ConnCreateParams -> authMode                   #} p (fe authMode)
    {#set ConnCreateParams -> connectionClass            #} p cc
    {#set ConnCreateParams -> connectionClassLength      #} p (fromIntegral cclen)
    {#set ConnCreateParams -> purity                     #} p (fe purity)
    {#set ConnCreateParams -> newPassword                #} p np
    {#set ConnCreateParams -> newPasswordLength          #} p (fromIntegral nplen)
    {#set ConnCreateParams -> appContext                 #} p appContext
    {#set ConnCreateParams -> numAppContext              #} p numAppContext
    {#set ConnCreateParams -> externalAuth               #} p externalAuth
    {#set ConnCreateParams -> externalHandle             #} p externalHandle
    {#set ConnCreateParams -> pool                       #} p pool
    {#set ConnCreateParams -> tag                        #} p tg
    {#set ConnCreateParams -> tagLength                  #} p (fromIntegral tglen)
    {#set ConnCreateParams -> matchAnyTag                #} p matchAnyTag
    {#set ConnCreateParams -> outTag                     #} p og
    {#set ConnCreateParams -> outTagLength               #} p (fromIntegral oglen)
    {#set ConnCreateParams -> outTagFound                #} p outTagFound
    {#set ConnCreateParams -> shardingKeyColumns         #} p shardingKeyColumns
    {#set ConnCreateParams -> numShardingKeyColumns      #} p numShardingKeyColumns
    {#set ConnCreateParams -> superShardingKeyColumns    #} p superShardingKeyColumns
    {#set ConnCreateParams -> numSuperShardingKeyColumns #} p numSuperShardingKeyColumns
  peek      p = do
    authMode                   <- te <$> {#get ConnCreateParams -> authMode                   #} p
    connectionClass'           <-        {#get ConnCreateParams -> connectionClass            #} p
    connectionClassLength      <-        {#get ConnCreateParams -> connectionClassLength      #} p
    purity                     <- te <$> {#get ConnCreateParams -> purity                     #} p
    newPassword'               <-        {#get ConnCreateParams -> newPassword                #} p
    newPasswordLength          <-        {#get ConnCreateParams -> newPasswordLength          #} p
    appContext                 <-        {#get ConnCreateParams -> appContext                 #} p
    numAppContext              <-        {#get ConnCreateParams -> numAppContext              #} p
    externalAuth               <-        {#get ConnCreateParams -> externalAuth               #} p
    externalHandle             <-        {#get ConnCreateParams -> externalHandle             #} p
    pool                       <-        {#get ConnCreateParams -> pool                       #} p
    tag'                       <-        {#get ConnCreateParams -> tag                        #} p
    tagLength                  <-        {#get ConnCreateParams -> tagLength                  #} p
    matchAnyTag                <-        {#get ConnCreateParams -> matchAnyTag                #} p
    outTag'                    <-        {#get ConnCreateParams -> outTag                     #} p
    outTagLength               <-        {#get ConnCreateParams -> outTagLength               #} p
    outTagFound                <-        {#get ConnCreateParams -> outTagFound                #} p
    shardingKeyColumns         <-        {#get ConnCreateParams -> shardingKeyColumns         #} p
    numShardingKeyColumns      <-        {#get ConnCreateParams -> numShardingKeyColumns      #} p
    superShardingKeyColumns    <-        {#get ConnCreateParams -> superShardingKeyColumns    #} p
    numSuperShardingKeyColumns <-        {#get ConnCreateParams -> numSuperShardingKeyColumns #} p
    let connectionClass = (connectionClass', fromIntegral connectionClassLength)
        newPassword     = (newPassword',     fromIntegral newPasswordLength)
        tag             = (tag',             fromIntegral tagLength)
        outTag          = (outTag',          fromIntegral outTagLength)
    return Data_ConnCreateParams {..}

data DataValue
  = DataNull           !NativeTypeNum
  | DataBFile          !(Ptr DPI_Lob)
  | DataBoolean        !Bool
  | DataBlob           !(Ptr DPI_Lob)
  | DataChar           !Data_Bytes
  | DataClob           !(Ptr DPI_Lob)
  | DataDate           !Data_Timestamp
  | DataIntervalDs     !Data_IntervalDS
  | DataIntervalYm     !Data_IntervalYM
  | DataLongRaw        !Data_Bytes
  | DataLongVarchar    !Data_Bytes
  | DataDouble         !CDouble
  | DataFloat          !CFloat
  | DataInt            !Int64
  | DataUint           !Word64
  | DataNChar          !Data_Bytes
  | DataNClob          !(Ptr DPI_Lob)
  | DataNumDouble      !Scientific
  | DataNumBytes       !Data_Bytes
  | DataNumInt         !Int64
  | DataNumUint        !Word64
  | DataNVarchar       !Data_Bytes
  | DataObject         !(Ptr DPI_Object)
  | DataRaw            !Data_Bytes
  | DataRowid          !(Ptr DPI_Rowid)
  | DataStmt           !(Ptr DPI_Stmt)
  | DataTimestamp      !Data_Timestamp
  | DataTimestampD     !CDouble
  | DataTimestampLtz   !Data_Timestamp
  | DataTimestampLtzD  !CDouble
  | DataTimestampTz    !Data_Timestamp
  | DataTimestampTzD   !CDouble
  | DataVarchar        !Data_Bytes
  deriving Show

{-# INLINE newData #-}
newData :: DataValue -> IO (NativeTypeNum, OracleTypeNum, PtrData)
newData d = do
  pd <- malloc
  let (tp,ot) = go d
  poke pd (Data $ \_ _ -> return d)
  return (tp, ot, pd)
  where
    {-# INLINE go #-}
    go (DataNull          t) = (t,                    OracleTypeNone         )
    go (DataBFile         _) = (NativeTypeLob,        OracleTypeBfile        )
    go (DataBoolean       _) = (NativeTypeBoolean,    OracleTypeBoolean      )
    go (DataBlob          _) = (NativeTypeLob,        OracleTypeBlob         )
    go (DataChar          _) = (NativeTypeBytes,      OracleTypeChar         )
    go (DataClob          _) = (NativeTypeLob,        OracleTypeClob         )
    go (DataDate          _) = (NativeTypeTimestamp,  OracleTypeDate         )
    go (DataIntervalDs    _) = (NativeTypeIntervalDs, OracleTypeIntervalDs   )
    go (DataIntervalYm    _) = (NativeTypeIntervalYm, OracleTypeIntervalYm   )
    go (DataLongRaw       _) = (NativeTypeBytes,      OracleTypeLongRaw      )
    go (DataLongVarchar   _) = (NativeTypeBytes,      OracleTypeLongVarchar  )
    go (DataDouble        _) = (NativeTypeDouble,     OracleTypeNativeDouble )
    go (DataFloat         _) = (NativeTypeFloat,      OracleTypeNativeFloat  )
    go (DataInt           _) = (NativeTypeInt64,      OracleTypeNativeInt    )
    go (DataUint          _) = (NativeTypeUint64,     OracleTypeNativeUint   )
    go (DataNChar         _) = (NativeTypeBytes,      OracleTypeNchar        )
    go (DataNClob         _) = (NativeTypeLob,        OracleTypeNclob        )
    go (DataNumDouble     _) = (NativeTypeDouble,     OracleTypeNumber       )
    go (DataNumBytes      _) = (NativeTypeBytes,      OracleTypeNumber       )
    go (DataNumInt        _) = (NativeTypeInt64,      OracleTypeNumber       )
    go (DataNumUint       _) = (NativeTypeUint64,     OracleTypeNumber       )
    go (DataNVarchar      _) = (NativeTypeBytes,      OracleTypeNvarchar     )
    go (DataObject        _) = (NativeTypeObject,     OracleTypeObject       )
    go (DataRaw           _) = (NativeTypeBytes,      OracleTypeRaw          )
    go (DataRowid         _) = (NativeTypeRowid,      OracleTypeRowid        )
    go (DataStmt          _) = (NativeTypeStmt,       OracleTypeStmt         )
    go (DataTimestamp     _) = (NativeTypeTimestamp,  OracleTypeTimestamp    )
    go (DataTimestampD    _) = (NativeTypeDouble,     OracleTypeTimestamp    )
    go (DataTimestampLtz  _) = (NativeTypeTimestamp,  OracleTypeTimestampLtz )
    go (DataTimestampLtzD _) = (NativeTypeDouble,     OracleTypeTimestampLtz )
    go (DataTimestampTz   _) = (NativeTypeTimestamp,  OracleTypeTimestampTz  )
    go (DataTimestampTzD  _) = (NativeTypeDouble,     OracleTypeTimestampTz  )
    go (DataVarchar       _) = (NativeTypeBytes,      OracleTypeVarchar      )

newtype Data = Data (NativeTypeNum -> OracleTypeNum -> IO DataValue)

instance Storable Data where
  sizeOf    _ = {#sizeof  Data #}
  alignment _ = {#alignof Data #}
  poke      p' (Data f) = do
    f NativeTypeDouble OracleTypeNone >>= go p'
    where
      sLob p1 v1 = {#set Data -> value.asLOB        #} p1 v1
      sByt p2 Data_Bytes{..} = do
        let (b,bl) = bytes
        {#set Data -> value.asBytes.ptr      #} p2 b
        {#set Data -> value.asBytes.length   #} p2 (fromIntegral bl)
        B.unsafeUseAsCString encoding $ {#set Data -> value.asBytes.encoding #} p2
      sTmp p3 Data_Timestamp{..} = do
        {#set Data -> value.asTimestamp.year           #} p3 $ fromIntegral year
        {#set Data -> value.asTimestamp.month          #} p3 $ fromIntegral month
        {#set Data -> value.asTimestamp.day            #} p3 $ fromIntegral day
        {#set Data -> value.asTimestamp.hour           #} p3 $ fromIntegral hour
        {#set Data -> value.asTimestamp.minute         #} p3 $ fromIntegral minute
        {#set Data -> value.asTimestamp.second         #} p3 $ fromIntegral second
        {#set Data -> value.asTimestamp.fsecond        #} p3 $ fromIntegral fsecond
        {#set Data -> value.asTimestamp.tzHourOffset   #} p3 0
        {#set Data -> value.asTimestamp.tzMinuteOffset #} p3 0
      go p (DataNull          _) = {#set Data -> isNull             #} p 1
      go p (DataBoolean       v) = {#set Data -> value.asBoolean    #} p (fromBool v)
      go p (DataIntervalDs    Data_IntervalDS{..}) = do
        {#set Data -> value.asIntervalDS.days     #} p $ fromIntegral days
        {#set Data -> value.asIntervalDS.hours    #} p $ fromIntegral hours
        {#set Data -> value.asIntervalDS.minutes  #} p $ fromIntegral minutes
        {#set Data -> value.asIntervalDS.seconds  #} p $ fromIntegral seconds
        {#set Data -> value.asIntervalDS.fseconds #} p $ fromIntegral fseconds
      go p (DataIntervalYm    (Data_IntervalYM {..})) = do
        {#set Data -> value.asIntervalYM.years    #} p  years
        {#set Data -> value.asIntervalYM.months   #} p  months
      go p (DataChar          v) = sByt p v
      go p (DataLongRaw       v) = sByt p v
      go p (DataLongVarchar   v) = sByt p v
      go p (DataNChar         v) = sByt p v
      go p (DataNumBytes      v) = sByt p v
      go p (DataNVarchar      v) = sByt p v
      go p (DataVarchar       v) = sByt p v
      go p (DataRaw           v) = sByt p v
      go p (DataTimestamp     v) = sTmp p v
      go p (DataTimestampLtz  v) = sTmp p v
      go p (DataTimestampTz   v) = sTmp p v
      go p (DataDate          v) = sTmp p v
      go p (DataBFile         v) = sLob p v
      go p (DataNClob         v) = sLob p v
      go p (DataBlob          v) = sLob p v
      go p (DataClob          v) = sLob p v
      go p (DataDouble        v) = {#set Data -> value.asDouble #} p v
      go p (DataFloat         v) = {#set Data -> value.asFloat  #} p v
      go p (DataInt           v) = {#set Data -> value.asInt64  #} p (fromIntegral v)
      go p (DataUint          v) = {#set Data -> value.asUint64 #} p (fromIntegral v)
      go p (DataNumDouble     v) = {#set Data -> value.asDouble #} p (realToFrac v)
      go p (DataNumInt        v) = {#set Data -> value.asInt64  #} p (fromIntegral v)
      go p (DataNumUint       v) = {#set Data -> value.asUint64 #} p (fromIntegral v)
      go p (DataObject        v) = {#set Data -> value.asObject #} p v
      go p (DataRowid         v) = {#set Data -> value.asRowid  #} p v
      go p (DataStmt          v) = {#set Data -> value.asStmt   #} p v
      go p (DataTimestampD    v) = {#set Data -> value.asDouble #} p v
      go p (DataTimestampLtzD v) = {#set Data -> value.asDouble #} p v
      go p (DataTimestampTzD  v) = {#set Data -> value.asDouble #} p v
  peek      = return . Data . goWithNullCheck
    where
      goWithNullCheck p t o = do
        n <- {#get Data -> isNull #} p
        if n == 1
          then pure $ DataNull t
          else go p t o

      go p NativeTypeBoolean    _ = (DataBoolean .toBool) <$> {#get Data -> value.asBoolean #} p
      go p NativeTypeInt64      o = (gInt o .ft)          <$> {#get Data -> value.asInt64   #} p
      go p NativeTypeUint64     o = (gUnt o .ft)          <$> {#get Data -> value.asUint64  #} p
      go p NativeTypeFloat      _ = DataFloat             <$> {#get Data -> value.asFloat   #} p
      go p NativeTypeDouble     _ = DataDouble            <$> {#get Data -> value.asDouble  #} p
      go p NativeTypeBytes      o = gByt o                <$> do
        ptr'     <- {#get Data -> value.asBytes.ptr      #} p
        len      <- {#get Data -> value.asBytes.length   #} p
        encoding <- {#get Data -> value.asBytes.encoding #} p >>= ts
        let bytes = (ptr', fromIntegral len)
        return Data_Bytes{..}
      go p NativeTypeTimestamp  o = do
        year           <- {#get Data -> value.asTimestamp.year           #} p
        month          <- {#get Data -> value.asTimestamp.month          #} p
        day            <- {#get Data -> value.asTimestamp.day            #} p
        hour           <- {#get Data -> value.asTimestamp.hour           #} p
        minute         <- {#get Data -> value.asTimestamp.minute         #} p
        second         <- {#get Data -> value.asTimestamp.second         #} p
        fsecond        <- {#get Data -> value.asTimestamp.fsecond        #} p
        tzHourOffset   <- {#get Data -> value.asTimestamp.tzHourOffset   #} p
        tzMinuteOffset <- {#get Data -> value.asTimestamp.tzMinuteOffset #} p
        return $ toTime o Data_Timestamp{..}
      go p NativeTypeIntervalDs _ = DataIntervalDs <$> do
        days     <- {#get Data -> value.asIntervalDS.days     #} p
        hours    <- {#get Data -> value.asIntervalDS.hours    #} p
        minutes  <- {#get Data -> value.asIntervalDS.minutes  #} p
        seconds  <- {#get Data -> value.asIntervalDS.seconds  #} p
        fseconds <- {#get Data -> value.asIntervalDS.fseconds #} p
        return $ Data_IntervalDS{..}
      go p NativeTypeIntervalYm _ = DataIntervalYm <$> do
        years    <- {#get Data -> value.asIntervalYM.years    #} p
        months   <- {#get Data -> value.asIntervalYM.months   #} p
        return Data_IntervalYM {..}
      go p NativeTypeLob        o = gLob o     <$> {#get Data -> value.asLOB    #} p
      go p NativeTypeObject     _ = DataObject <$> {#get Data -> value.asObject #} p
      go p NativeTypeStmt       _ = DataStmt   <$> {#get Data -> value.asStmt   #} p
      go p NativeTypeRowid      _ = DataRowid  <$> {#get Data -> value.asRowid  #} p


gByt OracleTypeChar         = DataChar
gByt OracleTypeLongRaw      = DataLongRaw
gByt OracleTypeLongVarchar  = DataLongVarchar
gByt OracleTypeNchar        = DataNChar
gByt OracleTypeNumber       = DataNumBytes
gByt OracleTypeNvarchar     = DataNVarchar
gByt OracleTypeRaw          = DataRaw
gByt OracleTypeVarchar      = DataVarchar
gByt _                      = DataVarchar
gInt OracleTypeNumber       = DataNumInt
gInt OracleTypeNativeInt    = DataInt
gInt _                      = DataInt
gUnt OracleTypeNumber       = DataNumUint
gUnt OracleTypeNativeUint   = DataUint
gUnt _                      = DataUint
gDbl OracleTypeNumber       = DataNumDouble . realToFrac
gDbl OracleTypeTimestamp    = DataTimestampD
gDbl OracleTypeTimestampLtz = DataTimestampLtzD
gDbl OracleTypeTimestampTz  = DataTimestampTzD
gDbl OracleTypeNativeDouble = DataTimestampTzD
gDbl _                      = DataDouble
gLob OracleTypeBfile        = DataBFile
gLob OracleTypeBlob         = DataBlob
gLob OracleTypeClob         = DataClob
gLob OracleTypeNclob        = DataNClob
gLob _                      = DataBlob
gTmp OracleTypeDate         = DataDate
gTmp OracleTypeTimestamp    = DataTimestamp
gTmp OracleTypeTimestampLtz = DataTimestampLtz
gTmp OracleTypeTimestampTz  = DataTimestampTz
gTmp _                      = DataTimestamp

data Data_DataTypeInfo  = Data_DataTypeInfo
  { oracleTypeNum        :: !OracleTypeNum
  , defaultNativeTypeNum :: !NativeTypeNum
  , ociTypeCode          :: !CUShort
  , dbSizeInBytes        :: !CUInt
  , clientSizeInBytes    :: !CUInt
  , sizeInChars          :: !CUInt
  , precision            :: !CShort
  , scale                :: !CSChar
  , fsPrecision          :: !CUChar
  , objectType           :: !(Ptr DPI_ObjectType)
  } deriving Show

instance Storable Data_DataTypeInfo where
  sizeOf    _ = {#sizeof  DataTypeInfo #}
  alignment _ = {#alignof DataTypeInfo #}
  poke    _ _ = noImplement
  peek      p = do
    oracleTypeNum        <- te <$> {#get DataTypeInfo -> oracleTypeNum        #} p
    defaultNativeTypeNum <- te <$> {#get DataTypeInfo -> defaultNativeTypeNum #} p
    ociTypeCode          <-        {#get DataTypeInfo -> ociTypeCode          #} p
    dbSizeInBytes        <-        {#get DataTypeInfo -> dbSizeInBytes        #} p
    clientSizeInBytes    <-        {#get DataTypeInfo -> clientSizeInBytes    #} p
    sizeInChars          <-        {#get DataTypeInfo -> sizeInChars          #} p
    precision            <-        {#get DataTypeInfo -> precision            #} p
    scale                <-        {#get DataTypeInfo -> scale                #} p
    fsPrecision          <-        {#get DataTypeInfo -> fsPrecision          #} p
    objectType           <-        {#get DataTypeInfo -> objectType           #} p
    return Data_DataTypeInfo {..}

{-# INLINE toTime #-}
toTime :: OracleTypeNum -> Data_Timestamp -> DataValue
toTime t v
  = let go OracleTypeDate         = DataDate
        go OracleTypeTimestamp    = DataTimestamp
        go OracleTypeTimestampLtz = DataTimestampLtz
        go OracleTypeTimestampTz  = DataTimestampTz
        go _                      = DataTimestamp
    in go t v


data Data_EncodingInfo  = Data_EncodingInfo
  { encoding              :: !ByteString
  , maxBytesPerCharacter  :: !CInt
  , nencoding             :: !ByteString
  , nmaxBytesPerCharacter :: !CInt
  } deriving Show

instance Storable Data_EncodingInfo where
  sizeOf    _ = {#sizeof  EncodingInfo #}
  alignment _ = {#alignof EncodingInfo #}
  poke   _  _ = noImplement
  peek      p = do
    encoding              <- {#get EncodingInfo -> encoding              #} p >>= ts
    maxBytesPerCharacter  <- {#get EncodingInfo -> maxBytesPerCharacter  #} p
    nencoding             <- {#get EncodingInfo -> nencoding             #} p >>= ts
    nmaxBytesPerCharacter <- {#get EncodingInfo -> nmaxBytesPerCharacter #} p
    return Data_EncodingInfo {..}

data Data_ErrorInfo  = Data_ErrorInfo
  { code          :: !CInt
  , offset        :: !CUShort
  , message       :: !ByteString
  , encoding      :: !ByteString
  , fnName        :: !ByteString
  , action        :: !ByteString
  , sqlState      :: !ByteString
  , isRecoverable :: !Bool
  } deriving Show

instance Storable Data_ErrorInfo where
  sizeOf    _ = {#sizeof  ErrorInfo #}
  alignment _ = {#alignof ErrorInfo #}
  poke    _ _ = noImplement
  peek      p = do
    code          <-            {#get ErrorInfo -> code          #} p
    offset        <-            {#get ErrorInfo -> offset        #} p
    message'      <-            {#get ErrorInfo -> message       #} p
    messageLength <-            {#get ErrorInfo -> messageLength #} p
    encoding      <-            {#get ErrorInfo -> encoding      #} p >>= ts
    fnName        <-            {#get ErrorInfo -> fnName        #} p >>= ts
    action        <-            {#get ErrorInfo -> action        #} p >>= ts
    sqlState      <-            {#get ErrorInfo -> sqlState      #} p >>= ts
    isRecoverable <- toBool <$> {#get ErrorInfo -> isRecoverable #} p
    message       <- tsLen (message', fromIntegral messageLength)
    return Data_ErrorInfo {..}

data Data_ObjectAttrInfo  = Data_ObjectAttrInfo
  { name       :: !CStringLen
  , typeInfo   :: !Data_DataTypeInfo
  } deriving Show

instance Storable Data_ObjectAttrInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke    _ _ = noImplement
  peek      p = do
    name'      <- {#get ObjectAttrInfo -> name       #} p
    nameLength <- {#get ObjectAttrInfo -> nameLength #} p
    typeInfo   <- do
      oracleTypeNum        <- te <$> {#get ObjectAttrInfo -> typeInfo.oracleTypeNum        #} p
      defaultNativeTypeNum <- te <$> {#get ObjectAttrInfo -> typeInfo.defaultNativeTypeNum #} p
      ociTypeCode          <-        {#get ObjectAttrInfo -> typeInfo.ociTypeCode          #} p
      dbSizeInBytes        <-        {#get ObjectAttrInfo -> typeInfo.dbSizeInBytes        #} p
      clientSizeInBytes    <-        {#get ObjectAttrInfo -> typeInfo.clientSizeInBytes    #} p
      sizeInChars          <-        {#get ObjectAttrInfo -> typeInfo.sizeInChars          #} p
      precision            <-        {#get ObjectAttrInfo -> typeInfo.precision            #} p
      scale                <-        {#get ObjectAttrInfo -> typeInfo.scale                #} p
      fsPrecision          <-        {#get ObjectAttrInfo -> typeInfo.fsPrecision          #} p
      objectType           <-        {#get ObjectAttrInfo -> typeInfo.objectType           #} p
      return Data_DataTypeInfo {..}
    let name       = (name', fromIntegral nameLength)
    return Data_ObjectAttrInfo {..}

data Data_ObjectTypeInfo  = Data_ObjectTypeInfo
  { schema          :: !CStringLen
  , name            :: !CStringLen
  , isCollection    :: !Bool
  , elementTypeInfo :: !Data_DataTypeInfo
  , numAttributes   :: !CUShort
  } deriving Show

instance Storable Data_ObjectTypeInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke    _ _ = noImplement
  peek      p = do
    schema'         <-             {#get ObjectTypeInfo -> schema          #} p
    schemaLength    <-             {#get ObjectTypeInfo -> schemaLength    #} p
    name'           <-             {#get ObjectTypeInfo -> name            #} p
    nameLength      <-             {#get ObjectTypeInfo -> nameLength      #} p
    isCollection    <- toBool  <$> {#get ObjectTypeInfo -> isCollection    #} p
    elementTypeInfo <- do
      oracleTypeNum        <- te <$> {#get ObjectTypeInfo -> elementTypeInfo.oracleTypeNum        #} p
      defaultNativeTypeNum <- te <$> {#get ObjectTypeInfo -> elementTypeInfo.defaultNativeTypeNum #} p
      ociTypeCode          <-        {#get ObjectTypeInfo -> elementTypeInfo.ociTypeCode          #} p
      dbSizeInBytes        <-        {#get ObjectTypeInfo -> elementTypeInfo.dbSizeInBytes        #} p
      clientSizeInBytes    <-        {#get ObjectTypeInfo -> elementTypeInfo.clientSizeInBytes    #} p
      sizeInChars          <-        {#get ObjectTypeInfo -> elementTypeInfo.sizeInChars          #} p
      precision            <-        {#get ObjectTypeInfo -> elementTypeInfo.precision            #} p
      scale                <-        {#get ObjectTypeInfo -> elementTypeInfo.scale                #} p
      fsPrecision          <-        {#get ObjectTypeInfo -> elementTypeInfo.fsPrecision          #} p
      objectType           <-        {#get ObjectTypeInfo -> elementTypeInfo.objectType           #} p
      return Data_DataTypeInfo {..}
    numAttributes   <- {#get ObjectTypeInfo -> numAttributes   #} p
    let schema       = (schema', fromIntegral schemaLength)
        name         = (name',   fromIntegral nameLength)
    return Data_ObjectTypeInfo {..}

data Data_PoolCreateParams  = Data_PoolCreateParams
  { minSessions        :: !CUInt
  , maxSessions        :: !CUInt
  , sessionIncrement   :: !CUInt
  , pingInterval       :: !CInt
  , pingTimeout        :: !CInt
  , homogeneous        :: !CInt
  , externalAuth       :: !CInt
  , getMode            :: !PoolGetMode
  , outPoolName        :: !CStringLen
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
  , timeout            :: !CUInt
  , waitTimeout        :: !CUInt
  , maxLifetimeSession :: !CUInt
#endif
  } deriving Show

instance Storable Data_PoolCreateParams where
  sizeOf    _ = {#sizeof  PoolCreateParams #}
  alignment _ = {#alignof PoolCreateParams #}
  poke      p Data_PoolCreateParams{..} = do
    let (e,elen) = outPoolName
    {#set PoolCreateParams -> minSessions       #} p minSessions
    {#set PoolCreateParams -> maxSessions       #} p maxSessions
    {#set PoolCreateParams -> sessionIncrement  #} p sessionIncrement
    {#set PoolCreateParams -> pingInterval      #} p pingInterval
    {#set PoolCreateParams -> pingTimeout       #} p pingTimeout
    {#set PoolCreateParams -> homogeneous       #} p homogeneous
    {#set PoolCreateParams -> externalAuth      #} p externalAuth
    {#set PoolCreateParams -> getMode           #} p (fe              getMode)
    {#set PoolCreateParams -> outPoolName       #} p e
    {#set PoolCreateParams -> outPoolNameLength #} p (fromIntegral    elen)
  peek      p = do
    minSessions       <-        {#get PoolCreateParams -> minSessions        #} p
    maxSessions       <-        {#get PoolCreateParams -> maxSessions        #} p
    sessionIncrement  <-        {#get PoolCreateParams -> sessionIncrement   #} p
    pingInterval      <-        {#get PoolCreateParams -> pingInterval       #} p
    pingTimeout       <-        {#get PoolCreateParams -> pingTimeout        #} p
    homogeneous       <-        {#get PoolCreateParams -> homogeneous        #} p
    externalAuth      <-        {#get PoolCreateParams -> externalAuth       #} p
    getMode           <- te <$> {#get PoolCreateParams -> getMode            #} p
    outPoolName'      <-        {#get PoolCreateParams -> outPoolName        #} p
    outPoolNameLength <-        {#get PoolCreateParams -> outPoolNameLength  #} p
    let outPoolName    = (outPoolName', fromIntegral outPoolNameLength)
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
    timeout           <-        {#get PoolCreateParams -> timeout            #} p
    waitTimeout       <-        {#get PoolCreateParams -> waitTimeout        #} p
    maxLifetimeSession<-        {#get PoolCreateParams -> maxLifetimeSession #} p
#endif
    return Data_PoolCreateParams {..}

data Data_QueryInfo = Data_QueryInfo
  { name       :: !CStringLen
  , typeInfo   :: !Data_DataTypeInfo
  , nullOk     :: !Bool
  } deriving Show

instance Storable Data_QueryInfo where
  sizeOf    _ = {#sizeof  QueryInfo #}
  alignment _ = {#alignof QueryInfo #}
  poke    _ _ = noImplement
  peek      p = do
    name'      <- {#get QueryInfo -> name       #} p
    nameLength <- {#get QueryInfo -> nameLength #} p
    typeInfo   <- do
      oracleTypeNum        <- te <$> {#get QueryInfo -> typeInfo.oracleTypeNum        #} p
      defaultNativeTypeNum <- te <$> {#get QueryInfo -> typeInfo.defaultNativeTypeNum #} p
      ociTypeCode          <-        {#get QueryInfo -> typeInfo.ociTypeCode          #} p
      dbSizeInBytes        <-        {#get QueryInfo -> typeInfo.dbSizeInBytes        #} p
      clientSizeInBytes    <-        {#get QueryInfo -> typeInfo.clientSizeInBytes    #} p
      sizeInChars          <-        {#get QueryInfo -> typeInfo.sizeInChars          #} p
      precision            <-        {#get QueryInfo -> typeInfo.precision            #} p
      scale                <-        {#get QueryInfo -> typeInfo.scale                #} p
      fsPrecision          <-        {#get QueryInfo -> typeInfo.fsPrecision          #} p
      objectType           <-        {#get QueryInfo -> typeInfo.objectType           #} p
      return Data_DataTypeInfo {..}
    nullOk     <- toBool  <$> {#get QueryInfo -> nullOk     #} p
    let name    = (name', fromIntegral nameLength)
    return Data_QueryInfo {..}

data Data_ShardingKeyColumn  = Data_ShardingKeyColumn
  { oracleTypeNum :: !OracleTypeNum
  , nativeTypeNum :: !NativeTypeNum
  , value         :: !DataValue
  } deriving Show

instance Storable Data_ShardingKeyColumn where
  sizeOf    _ = {#sizeof  ShardingKeyColumn #}
  alignment _ = {#alignof ShardingKeyColumn #}
  poke    _ _ = noImplement
  peek     p' = do
    oracleTypeNum <- te      <$> {#get ShardingKeyColumn -> oracleTypeNum #} p'
    nativeTypeNum <- te      <$> {#get ShardingKeyColumn -> nativeTypeNum #} p'
    value         <- go p' nativeTypeNum oracleTypeNum
    return Data_ShardingKeyColumn {..}
    where
      go p NativeTypeInt64  o = (gInt o .ft) <$> {#get ShardingKeyColumn -> value.asInt64      #} p
      go p NativeTypeUint64 o = (gUnt o .ft) <$> {#get ShardingKeyColumn -> value.asUint64     #} p
      go p NativeTypeFloat  _ = DataFloat    <$> {#get ShardingKeyColumn -> value.asFloat      #} p
      go p NativeTypeDouble o = gDbl o       <$> {#get ShardingKeyColumn -> value.asDouble     #} p
      go p NativeTypeBytes  o = gByt o       <$> do
        ptr'     <- {#get ShardingKeyColumn -> value.asBytes.ptr      #} p
        len      <- {#get ShardingKeyColumn -> value.asBytes.length   #} p
        encoding <- {#get ShardingKeyColumn -> value.asBytes.encoding #} p >>= ts
        let bytes = (ptr', fromIntegral len)
        return Data_Bytes{..}
      go p NativeTypeTimestamp  t = do
        year           <- {#get ShardingKeyColumn -> value.asTimestamp.year           #} p
        month          <- {#get ShardingKeyColumn -> value.asTimestamp.month          #} p
        day            <- {#get ShardingKeyColumn -> value.asTimestamp.day            #} p
        hour           <- {#get ShardingKeyColumn -> value.asTimestamp.hour           #} p
        minute         <- {#get ShardingKeyColumn -> value.asTimestamp.minute         #} p
        second         <- {#get ShardingKeyColumn -> value.asTimestamp.second         #} p
        fsecond        <- {#get ShardingKeyColumn -> value.asTimestamp.fsecond        #} p
        tzHourOffset   <- {#get ShardingKeyColumn -> value.asTimestamp.tzHourOffset   #} p
        tzMinuteOffset <- {#get ShardingKeyColumn -> value.asTimestamp.tzMinuteOffset #} p
        return $ toTime t Data_Timestamp{..}
      go p NativeTypeIntervalDs _ = DataIntervalDs <$> do
        days     <- {#get ShardingKeyColumn -> value.asIntervalDS.days     #} p
        hours    <- {#get ShardingKeyColumn -> value.asIntervalDS.hours    #} p
        minutes  <- {#get ShardingKeyColumn -> value.asIntervalDS.minutes  #} p
        seconds  <- {#get ShardingKeyColumn -> value.asIntervalDS.seconds  #} p
        fseconds <- {#get ShardingKeyColumn -> value.asIntervalDS.fseconds #} p
        return $ Data_IntervalDS{..}
      go p NativeTypeIntervalYm _ = DataIntervalYm <$> do
        years    <- {#get ShardingKeyColumn -> value.asIntervalYM.years    #} p
        months   <- {#get ShardingKeyColumn -> value.asIntervalYM.months   #} p
        return Data_IntervalYM {..}
      go p NativeTypeLob        o = gLob o                <$> {#get ShardingKeyColumn -> value.asLOB        #} p
      go p NativeTypeObject     _ = DataObject            <$> {#get ShardingKeyColumn -> value.asObject     #} p
      go p NativeTypeStmt       _ = DataStmt              <$> {#get ShardingKeyColumn -> value.asStmt       #} p
      go p NativeTypeRowid      _ = DataRowid             <$> {#get ShardingKeyColumn -> value.asRowid      #} p
      go p NativeTypeBoolean    _ = (DataBoolean .toBool) <$> {#get ShardingKeyColumn -> value.asBoolean    #} p

data Data_StmtInfo = Data_StmtInfo
  { isQuery       :: !Bool
  , isPLSQL       :: !Bool
  , isDDL         :: !Bool
  , isDML         :: !Bool
  , statementType :: !StatementType
  , isReturning   :: !Bool
  } deriving Show

instance Storable Data_StmtInfo where
  sizeOf    _ = {#sizeof  StmtInfo #}
  alignment _ = {#alignof StmtInfo #}
  poke    _ _ = noImplement
  peek      p = do
    isQuery       <- toBool <$> {#get StmtInfo -> isQuery       #} p
    isPLSQL       <- toBool <$> {#get StmtInfo -> isPLSQL       #} p
    isDDL         <- toBool <$> {#get StmtInfo -> isDDL         #} p
    isDML         <- toBool <$> {#get StmtInfo -> isDML         #} p
    statementType <- te     <$> {#get StmtInfo -> statementType #} p
    isReturning   <- toBool <$> {#get StmtInfo -> isReturning   #} p
    return Data_StmtInfo {..}

data Data_SubscrCreateParams = Data_SubscrCreateParams
  { subscrNamespace     :: !SubscrNamespace
  , protocol            :: !SubscrProtocol
  , qos                 :: !SubscrQOS
  , operations          :: !CInt
  , portNumber          :: !CUInt
  , timeout             :: !CUInt
  , name                :: !CStringLen
  , callback            :: !(FunPtr (Ptr () -> PtrSubscrMessage -> IO ()))
  , callbackContext     :: !(Ptr ())
  , recipientName       :: !CStringLen
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
  , ipAddress           :: !CStringLen
  , groupingClass       :: !SubscrGroupingClass
  , groupingValue       :: !CUInt
  , groupingType        :: !SubscrGroupingType
#endif
  } deriving Show

instance Storable Data_SubscrCreateParams where
  sizeOf    _ = {#sizeof  SubscrCreateParams #}
  alignment _ = {#alignof SubscrCreateParams #}
  poke   _  _ = noImplement
  peek      p = do
    subscrNamespace     <- te <$> {#get SubscrCreateParams -> subscrNamespace     #} p
    protocol            <- te <$> {#get SubscrCreateParams -> protocol            #} p
    qos                 <- te <$> {#get SubscrCreateParams -> qos                 #} p
    operations          <-        {#get SubscrCreateParams -> operations          #} p
    portNumber          <-        {#get SubscrCreateParams -> portNumber          #} p
    timeout             <-        {#get SubscrCreateParams -> timeout             #} p
    name'               <-        {#get SubscrCreateParams -> name                #} p
    nameLength          <-        {#get SubscrCreateParams -> nameLength          #} p
    callback            <-        {#get SubscrCreateParams -> callback            #} p
    callbackContext     <-        {#get SubscrCreateParams -> callbackContext     #} p
    recipientName'      <-        {#get SubscrCreateParams -> recipientName       #} p
    recipientNameLength <-        {#get SubscrCreateParams -> recipientNameLength #} p
    let name          = (name',          fromIntegral nameLength)
        recipientName = (recipientName', fromIntegral recipientNameLength)
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
    ipAddress'          <-        {#get SubscrCreateParams -> ipAddress           #} p
    ipAddressLength     <-        {#get SubscrCreateParams -> ipAddressLength     #} p
    groupingClass       <- te <$> {#get SubscrCreateParams -> groupingClass       #} p
    groupingValue       <-        {#get SubscrCreateParams -> groupingValue       #} p
    groupingType        <- te <$> {#get SubscrCreateParams -> groupingType        #} p
    let ipAddress       = (ipAddress', fromIntegral ipAddressLength)
#endif
    return Data_SubscrCreateParams {..}

data Data_SubscrMessage = Data_SubscrMessage
  { eventType    :: !EventType
  , dbName       :: !CStringLen
  , tables       :: !PtrSubscrMessageTable
  , numTables    :: !CUInt
  , queries      :: !PtrSubscrMessageQuery
  , numQueries   :: !CUInt
  , errorInfo    :: !PtrErrorInfo
  , txId         :: !(Ptr ())
  , txIdLength   :: !CUInt
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
  , registered   :: !CInt
  , queueName    :: !CStringLen
  , consumerName :: !CStringLen
#endif
  } deriving Show

instance Storable Data_SubscrMessage where
  sizeOf    _ = {#sizeof  SubscrMessage #}
  alignment _ = {#alignof SubscrMessage #}
  poke    _ _ = noImplement
  peek      p = do
    eventType          <- te <$> {#get SubscrMessage -> eventType          #} p
    dbName'            <-        {#get SubscrMessage -> dbName             #} p
    dbNameLength       <-        {#get SubscrMessage -> dbNameLength       #} p
    tables             <-        {#get SubscrMessage -> tables             #} p
    numTables          <-        {#get SubscrMessage -> numTables          #} p
    queries            <-        {#get SubscrMessage -> queries            #} p
    numQueries         <-        {#get SubscrMessage -> numQueries         #} p
    errorInfo          <-        {#get SubscrMessage -> errorInfo          #} p
    txId               <-        {#get SubscrMessage -> txId               #} p
    txIdLength         <-        {#get SubscrMessage -> txIdLength         #} p
    let dbName         = (dbName', fromIntegral dbNameLength)
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
    registered         <-        {#get SubscrMessage -> registered         #} p
    queueName'         <-        {#get SubscrMessage -> queueName          #} p
    queueNameLength    <-        {#get SubscrMessage -> queueNameLength    #} p
    consumerName'      <-        {#get SubscrMessage -> consumerName       #} p
    consumerNameLength <-        {#get SubscrMessage -> consumerNameLength #} p
    let queueName      = (queueName', fromIntegral queueNameLength)
        consumerName   = (consumerName',fromIntegral consumerNameLength)
#endif
    return Data_SubscrMessage {..}

data Data_SubscrMessageQuery = Data_SubscrMessageQuery
  { mid       :: !Int64
  , operation :: !OpCode
  , tables    :: !PtrSubscrMessageTable
  , numTables :: !CUInt
  } deriving Show

instance Storable Data_SubscrMessageQuery where
  sizeOf    _ = {#sizeof  SubscrMessageQuery #}
  alignment _ = {#alignof SubscrMessageQuery #}
  poke    _ _ = noImplement
  peek      p = do
    mid       <- ft <$> {#get SubscrMessageQuery -> id        #} p
    operation <- te <$> {#get SubscrMessageQuery -> operation #} p
    tables    <-        {#get SubscrMessageQuery -> tables    #} p
    numTables <-        {#get SubscrMessageQuery -> numTables #} p
    return Data_SubscrMessageQuery {..}

data Data_SubscrMessageRow = Data_SubscrMessageRow
  { operation   :: !OpCode
  , rowid       :: !CStringLen
  } deriving Show

instance Storable Data_SubscrMessageRow where
  sizeOf    _ = {#sizeof  SubscrMessageRow #}
  alignment _ = {#alignof SubscrMessageRow #}
  poke    _ _ = noImplement
  peek      p = do
    operation   <- te <$> {#get SubscrMessageRow -> operation   #} p
    rowid'      <-        {#get SubscrMessageRow -> rowid       #} p
    rowidLength <-        {#get SubscrMessageRow -> rowidLength #} p
    let rowid   = (rowid', fromIntegral rowidLength)
    return Data_SubscrMessageRow {..}

data Data_SubscrMessageTable = Data_SubscrMessageTable
  { operation  :: !OpCode
  , name       :: !CStringLen
  , rows       :: !PtrSubscrMessageRow
  , numRows    :: !CUInt
  } deriving Show

instance Storable Data_SubscrMessageTable where
  sizeOf    _ = {#sizeof  SubscrMessageTable #}
  alignment _ = {#alignof SubscrMessageTable #}
  poke    _ _ = noImplement
  peek      p = do
    operation  <- te <$> {#get SubscrMessageTable -> operation  #} p
    name'      <-        {#get SubscrMessageTable -> name       #} p
    nameLength <-        {#get SubscrMessageTable -> nameLength #} p
    rows       <-        {#get SubscrMessageTable -> rows       #} p
    numRows    <-        {#get SubscrMessageTable -> numRows    #} p
    let name    = (name', fromIntegral nameLength)
    return Data_SubscrMessageTable {..}

data Data_VersionInfo = Data_VersionInfo
  { versionNum     :: !CInt
  , releaseNum     :: !CInt
  , updateNum      :: !CInt
  , portReleaseNum :: !CInt
  , portUpdateNum  :: !CInt
  , fullVersionNum :: !CUInt
  } deriving Show

instance Storable Data_VersionInfo where
  sizeOf    _ = {#sizeof  VersionInfo #}
  alignment _ = {#alignof VersionInfo #}
  poke    _ _ = noImplement
  peek      p = do
    versionNum     <- {#get VersionInfo -> versionNum     #} p
    releaseNum     <- {#get VersionInfo -> releaseNum     #} p
    updateNum      <- {#get VersionInfo -> updateNum      #} p
    portReleaseNum <- {#get VersionInfo -> portReleaseNum #} p
    portUpdateNum  <- {#get VersionInfo -> portUpdateNum  #} p
    fullVersionNum <- {#get VersionInfo -> fullVersionNum #} p
    return Data_VersionInfo {..}

-- Context
{-# INLINE libContextCreate                 #-}
{-# INLINE libContextDestroy                #-}
{-# INLINE libContextGetClientVersion       #-}
{-# INLINE libContextInitCommonCreateParams #-}
{-# INLINE libContextInitConnCreateParams   #-}
{-# INLINE libContextInitPoolCreateParams   #-}
{-# INLINE libContextInitSubscrCreateParams #-}
{-# INLINE libContextGetError               #-}
libContextCreate                 = {#call Context_create                 #}
libContextDestroy                = {#call Context_destroy                #}
libContextGetClientVersion       = {#call Context_getClientVersion       #}
libContextInitCommonCreateParams = {#call Context_initCommonCreateParams #}
libContextInitConnCreateParams   = {#call Context_initConnCreateParams   #}
libContextInitPoolCreateParams   = {#call Context_initPoolCreateParams   #}
libContextInitSubscrCreateParams = {#call Context_initSubscrCreateParams #}
libContextGetError               = {#call Context_getError               #}

-- Conn
{-# INLINE libConnAddRef              #-}
{-# INLINE libConnBeginDistribTrans   #-}
{-# INLINE libConnBreakExecution      #-}
{-# INLINE libConnChangePassword      #-}
{-# INLINE libConnClose               #-}
{-# INLINE libConnCommit              #-}
{-# INLINE libConnCreate              #-}
{-# INLINE libConnDeqObject           #-}
{-# INLINE libConnEnqObject           #-}
{-# INLINE libConnGetCurrentSchema    #-}
{-# INLINE libConnGetEdition          #-}
{-# INLINE libConnGetEncodingInfo     #-}
{-# INLINE libConnGetExternalName     #-}
{-# INLINE libConnGetHandle           #-}
{-# INLINE libConnGetInternalName     #-}
{-# INLINE libConnGetLTXID            #-}
{-# INLINE libConnGetObjectType       #-}
{-# INLINE libConnGetServerVersion    #-}
{-# INLINE libConnGetStmtCacheSize    #-}
{-# INLINE libConnNewDeqOptions       #-}
{-# INLINE libConnNewEnqOptions       #-}
{-# INLINE libConnNewMsgProps         #-}
{-# INLINE libConnNewSubscription     #-}
{-# INLINE libConnNewTempLob          #-}
{-# INLINE libConnNewVar              #-}
{-# INLINE libConnPing                #-}
{-# INLINE libConnPrepareDistribTrans #-}
{-# INLINE libConnPrepareStmt         #-}
{-# INLINE libConnRelease             #-}
{-# INLINE libConnRollback            #-}
{-# INLINE libConnSetAction           #-}
{-# INLINE libConnSetClientIdentifier #-}
{-# INLINE libConnSetClientInfo       #-}
{-# INLINE libConnSetCurrentSchema    #-}
{-# INLINE libConnSetDbOp             #-}
{-# INLINE libConnSetExternalName     #-}
{-# INLINE libConnSetInternalName     #-}
{-# INLINE libConnSetModule           #-}
{-# INLINE libConnSetStmtCacheSize    #-}
{-# INLINE libConnShutdownDatabase    #-}
{-# INLINE libConnStartupDatabase     #-}
{-# INLINE libConnSubscribe           #-}
{-# INLINE libConnUnsubscribe         #-}
libConnAddRef              = {#call Conn_addRef              #}
libConnBeginDistribTrans   = {#call Conn_beginDistribTrans   #}
libConnBreakExecution      = {#call Conn_breakExecution      #}
libConnChangePassword      = {#call Conn_changePassword      #}
libConnClose               = {#call Conn_close               #}
libConnCommit              = {#call Conn_commit              #}
libConnCreate              = {#call Conn_create              #}
libConnDeqObject           = {#call Conn_deqObject           #}
libConnEnqObject           = {#call Conn_enqObject           #}
libConnGetCurrentSchema    = {#call Conn_getCurrentSchema    #}
libConnGetEdition          = {#call Conn_getEdition          #}
libConnGetEncodingInfo     = {#call Conn_getEncodingInfo     #}
libConnGetExternalName     = {#call Conn_getExternalName     #}
libConnGetHandle           = {#call Conn_getHandle           #}
libConnGetInternalName     = {#call Conn_getInternalName     #}
libConnGetLTXID            = {#call Conn_getLTXID            #}
libConnGetObjectType       = {#call Conn_getObjectType       #}
libConnGetServerVersion    = {#call Conn_getServerVersion    #}
libConnGetStmtCacheSize    = {#call Conn_getStmtCacheSize    #}
libConnNewDeqOptions       = {#call Conn_newDeqOptions       #}
libConnNewEnqOptions       = {#call Conn_newEnqOptions       #}
libConnNewMsgProps         = {#call Conn_newMsgProps         #}
#if DPI_MAJOR_VERSION >= 3
libConnNewSubscription     = {#call dpiConn_subscribe        #}
#else
libConnNewSubscription     = {#call Conn_newSubscription     #}
#endif
libConnNewTempLob          = {#call Conn_newTempLob          #}
libConnNewVar              = {#call Conn_newVar              #}
libConnPing                = {#call Conn_ping                #}
libConnPrepareDistribTrans = {#call Conn_prepareDistribTrans #}
libConnPrepareStmt         = {#call Conn_prepareStmt         #}
libConnRelease             = {#call Conn_release             #}
libConnRollback            = {#call Conn_rollback            #}
libConnSetAction           = {#call Conn_setAction           #}
libConnSetClientIdentifier = {#call Conn_setClientIdentifier #}
libConnSetClientInfo       = {#call Conn_setClientInfo       #}
libConnSetCurrentSchema    = {#call Conn_setCurrentSchema    #}
libConnSetDbOp             = {#call Conn_setDbOp             #}
libConnSetExternalName     = {#call Conn_setExternalName     #}
libConnSetInternalName     = {#call Conn_setInternalName     #}
libConnSetModule           = {#call Conn_setModule           #}
libConnSetStmtCacheSize    = {#call Conn_setStmtCacheSize    #}
libConnShutdownDatabase    = {#call Conn_shutdownDatabase    #}
libConnStartupDatabase     = {#call Conn_startupDatabase     #}
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
libConnSubscribe           = {#call Conn_subscribe           #}
libConnUnsubscribe         = {#call Conn_unsubscribe         #}
#else
libConnSubscribe           = error "No implement until 2.4.0"
libConnUnsubscribe         = error "No implement until 2.4.0"
#endif

-- Data
{-# INLINE libDataGetDouble     #-}
{-# INLINE libDataGetBytes      #-}
{-# INLINE libDataGetIntervalDS #-}
{-# INLINE libDataGetIntervalYM #-}
{-# INLINE libDataGetLOB        #-}
{-# INLINE libDataGetObject     #-}
{-# INLINE libDataGetStmt       #-}
{-# INLINE libDataGetTimestamp  #-}
{-# INLINE libDataGetFloat      #-}
{-# INLINE libDataGetBool       #-}
{-# INLINE libDataGetInt64      #-}
{-# INLINE libDataGetUint64     #-}
{-# INLINE libDataSetBool       #-}
{-# INLINE libDataSetBytes      #-}
{-# INLINE libDataSetDouble     #-}
{-# INLINE libDataSetFloat      #-}
{-# INLINE libDataSetInt64      #-}
{-# INLINE libDataSetIntervalDS #-}
{-# INLINE libDataSetIntervalYM #-}
{-# INLINE libDataSetLOB        #-}
{-# INLINE libDataSetObject     #-}
{-# INLINE libDataSetStmt       #-}
{-# INLINE libDataSetTimestamp  #-}
{-# INLINE libDataSetUint64     #-}
libDataGetDouble     = {#call Data_getDouble     #}
libDataGetBytes      = {#call Data_getBytes      #}
libDataGetIntervalDS = {#call Data_getIntervalDS #}
libDataGetIntervalYM = {#call Data_getIntervalYM #}
libDataGetLOB        = {#call Data_getLOB        #}
libDataGetObject     = {#call Data_getObject     #}
libDataGetStmt       = {#call Data_getStmt       #}
libDataGetTimestamp  = {#call Data_getTimestamp  #}
libDataGetFloat      = {#call Data_getFloat      #}
libDataGetBool       = {#call Data_getBool       #}
libDataGetInt64      = {#call Data_getInt64      #}
libDataGetUint64     = {#call Data_getUint64     #}
libDataSetBool       = {#call Data_setBool       #}
libDataSetBytes      = {#call Data_setBytes      #}
libDataSetDouble     = {#call Data_setDouble     #}
libDataSetFloat      = {#call Data_setFloat      #}
libDataSetInt64      = {#call Data_setInt64      #}
libDataSetIntervalDS = {#call Data_setIntervalDS #}
libDataSetIntervalYM = {#call Data_setIntervalYM #}
libDataSetLOB        = {#call Data_setLOB        #}
libDataSetObject     = {#call Data_setObject     #}
libDataSetStmt       = {#call Data_setStmt       #}
libDataSetTimestamp  = {#call Data_setTimestamp  #}
libDataSetUint64     = {#call Data_setUint64     #}


-- DeqOptions
{-# INLINE libDeqOptionsAddRef            #-}
{-# INLINE libDeqOptionsGetCondition      #-}
{-# INLINE libDeqOptionsGetConsumerName   #-}
{-# INLINE libDeqOptionsGetCorrelation    #-}
{-# INLINE libDeqOptionsGetMode           #-}
{-# INLINE libDeqOptionsGetMsgId          #-}
{-# INLINE libDeqOptionsGetNavigation     #-}
{-# INLINE libDeqOptionsGetTransformation #-}
{-# INLINE libDeqOptionsGetVisibility     #-}
{-# INLINE libDeqOptionsGetWait           #-}
{-# INLINE libDeqOptionsRelease           #-}
{-# INLINE libDeqOptionsSetCondition      #-}
{-# INLINE libDeqOptionsSetConsumerName   #-}
{-# INLINE libDeqOptionsSetCorrelation    #-}
{-# INLINE libDeqOptionsSetDeliveryMode   #-}
{-# INLINE libDeqOptionsSetMode           #-}
{-# INLINE libDeqOptionsSetMsgId          #-}
{-# INLINE libDeqOptionsSetNavigation     #-}
{-# INLINE libDeqOptionsSetTransformation #-}
{-# INLINE libDeqOptionsSetVisibility     #-}
{-# INLINE libDeqOptionsSetWait           #-}
libDeqOptionsAddRef            = {#call DeqOptions_addRef            #}
libDeqOptionsGetCondition      = {#call DeqOptions_getCondition      #}
libDeqOptionsGetConsumerName   = {#call DeqOptions_getConsumerName   #}
libDeqOptionsGetCorrelation    = {#call DeqOptions_getCorrelation    #}
libDeqOptionsGetMode           = {#call DeqOptions_getMode           #}
libDeqOptionsGetMsgId          = {#call DeqOptions_getMsgId          #}
libDeqOptionsGetNavigation     = {#call DeqOptions_getNavigation     #}
libDeqOptionsGetTransformation = {#call DeqOptions_getTransformation #}
libDeqOptionsGetVisibility     = {#call DeqOptions_getVisibility     #}
libDeqOptionsGetWait           = {#call DeqOptions_getWait           #}
libDeqOptionsRelease           = {#call DeqOptions_release           #}
libDeqOptionsSetCondition      = {#call DeqOptions_setCondition      #}
libDeqOptionsSetConsumerName   = {#call DeqOptions_setConsumerName   #}
libDeqOptionsSetCorrelation    = {#call DeqOptions_setCorrelation    #}
libDeqOptionsSetDeliveryMode   = {#call DeqOptions_setDeliveryMode   #}
libDeqOptionsSetMode           = {#call DeqOptions_setMode           #}
libDeqOptionsSetMsgId          = {#call DeqOptions_setMsgId          #}
libDeqOptionsSetNavigation     = {#call DeqOptions_setNavigation     #}
libDeqOptionsSetTransformation = {#call DeqOptions_setTransformation #}
libDeqOptionsSetVisibility     = {#call DeqOptions_setVisibility     #}
libDeqOptionsSetWait           = {#call DeqOptions_setWait           #}

-- EnqOptions
{-# INLINE libEnqOptionsAddRef            #-}
{-# INLINE libEnqOptionsGetTransformation #-}
{-# INLINE libEnqOptionsGetVisibility     #-}
{-# INLINE libEnqOptionsRelease           #-}
{-# INLINE libEnqOptionsSetDeliveryMode   #-}
{-# INLINE libEnqOptionsSetTransformation #-}
{-# INLINE libEnqOptionsSetVisibility     #-}
libEnqOptionsAddRef            = {#call EnqOptions_addRef            #}
libEnqOptionsGetTransformation = {#call EnqOptions_getTransformation #}
libEnqOptionsGetVisibility     = {#call EnqOptions_getVisibility     #}
libEnqOptionsRelease           = {#call EnqOptions_release           #}
libEnqOptionsSetDeliveryMode   = {#call EnqOptions_setDeliveryMode   #}
libEnqOptionsSetTransformation = {#call EnqOptions_setTransformation #}
libEnqOptionsSetVisibility     = {#call EnqOptions_setVisibility     #}

-- Lob
{-# INLINE libLobAddRef                  #-}
{-# INLINE libLobClose                   #-}
{-# INLINE libLobCloseResource           #-}
{-# INLINE libLobCopy                    #-}
{-# INLINE libLobFlushBuffer             #-}
{-# INLINE libLobGetBufferSize           #-}
{-# INLINE libLobGetChunkSize            #-}
{-# INLINE libLobGetDirectoryAndFileName #-}
{-# INLINE libLobGetFileExists           #-}
{-# INLINE libLobGetIsResourceOpen       #-}
{-# INLINE libLobGetSize                 #-}
{-# INLINE libLobOpenResource            #-}
{-# INLINE libLobReadBytes               #-}
{-# INLINE libLobRelease                 #-}
{-# INLINE libLobSetDirectoryAndFileName #-}
{-# INLINE libLobSetFromBytes            #-}
{-# INLINE libLobTrim                    #-}
{-# INLINE libLobWriteBytes              #-}
libLobAddRef                  = {#call Lob_addRef                  #}
libLobClose                   = {#call Lob_close                   #}
libLobCloseResource           = {#call Lob_closeResource           #}
libLobCopy                    = {#call Lob_copy                    #}
libLobFlushBuffer             = {#call Lob_flushBuffer             #}
libLobGetBufferSize           = {#call Lob_getBufferSize           #}
libLobGetChunkSize            = {#call Lob_getChunkSize            #}
libLobGetDirectoryAndFileName = {#call Lob_getDirectoryAndFileName #}
libLobGetFileExists           = {#call Lob_getFileExists           #}
libLobGetIsResourceOpen       = {#call Lob_getIsResourceOpen       #}
libLobGetSize                 = {#call Lob_getSize                 #}
libLobOpenResource            = {#call Lob_openResource            #}
libLobReadBytes               = {#call Lob_readBytes               #}
libLobRelease                 = {#call Lob_release                 #}
libLobSetDirectoryAndFileName = {#call Lob_setDirectoryAndFileName #}
libLobSetFromBytes            = {#call Lob_setFromBytes            #}
libLobTrim                    = {#call Lob_trim                    #}
libLobWriteBytes              = {#call Lob_writeBytes              #}

-- MsgProps
{-# INLINE libMsgPropsAddRef           #-}
{-# INLINE libMsgPropsGetCorrelation   #-}
{-# INLINE libMsgPropsGetDelay         #-}
{-# INLINE libMsgPropsGetDeliveryMode  #-}
{-# INLINE libMsgPropsGetEnqTime       #-}
{-# INLINE libMsgPropsGetExceptionQ    #-}
{-# INLINE libMsgPropsGetExpiration    #-}
{-# INLINE libMsgPropsGetNumAttempts   #-}
{-# INLINE libMsgPropsGetOriginalMsgId #-}
{-# INLINE libMsgPropsGetPriority      #-}
{-# INLINE libMsgPropsGetState         #-}
{-# INLINE libMsgPropsRelease          #-}
{-# INLINE libMsgPropsSetCorrelation   #-}
{-# INLINE libMsgPropsSetDelay         #-}
{-# INLINE libMsgPropsSetExceptionQ    #-}
{-# INLINE libMsgPropsSetExpiration    #-}
{-# INLINE libMsgPropsSetOriginalMsgId #-}
{-# INLINE libMsgPropsSetPriority      #-}
libMsgPropsAddRef           = {#call MsgProps_addRef           #}
libMsgPropsGetCorrelation   = {#call MsgProps_getCorrelation   #}
libMsgPropsGetDelay         = {#call MsgProps_getDelay         #}
libMsgPropsGetDeliveryMode  = {#call MsgProps_getDeliveryMode  #}
libMsgPropsGetEnqTime       = {#call MsgProps_getEnqTime       #}
libMsgPropsGetExceptionQ    = {#call MsgProps_getExceptionQ    #}
libMsgPropsGetExpiration    = {#call MsgProps_getExpiration    #}
libMsgPropsGetNumAttempts   = {#call MsgProps_getNumAttempts   #}
libMsgPropsGetOriginalMsgId = {#call MsgProps_getOriginalMsgId #}
libMsgPropsGetPriority      = {#call MsgProps_getPriority      #}
libMsgPropsGetState         = {#call MsgProps_getState         #}
libMsgPropsRelease          = {#call MsgProps_release          #}
libMsgPropsSetCorrelation   = {#call MsgProps_setCorrelation   #}
libMsgPropsSetDelay         = {#call MsgProps_setDelay         #}
libMsgPropsSetExceptionQ    = {#call MsgProps_setExceptionQ    #}
libMsgPropsSetExpiration    = {#call MsgProps_setExpiration    #}
libMsgPropsSetOriginalMsgId = {#call MsgProps_setOriginalMsgId #}
libMsgPropsSetPriority      = {#call MsgProps_setPriority      #}

-- Object
{-# INLINE libObjectAddRef                  #-}
{-# INLINE libObjectAppendElement           #-}
{-# INLINE libObjectCopy                    #-}
{-# INLINE libObjectDeleteElementByIndex    #-}
{-# INLINE libObjectGetAttributeValue       #-}
{-# INLINE libObjectGetElementExistsByIndex #-}
{-# INLINE libObjectGetElementValueByIndex  #-}
{-# INLINE libObjectGetFirstIndex           #-}
{-# INLINE libObjectGetLastIndex            #-}
{-# INLINE libObjectGetNextIndex            #-}
{-# INLINE libObjectGetPrevIndex            #-}
{-# INLINE libObjectGetSize                 #-}
{-# INLINE libObjectRelease                 #-}
{-# INLINE libObjectSetAttributeValue       #-}
{-# INLINE libObjectSetElementValueByIndex  #-}
{-# INLINE libObjectTrim                    #-}
libObjectAddRef                  = {#call Object_addRef                  #}
libObjectAppendElement           = {#call Object_appendElement           #}
libObjectCopy                    = {#call Object_copy                    #}
libObjectDeleteElementByIndex    = {#call Object_deleteElementByIndex    #}
libObjectGetAttributeValue       = {#call Object_getAttributeValue       #}
libObjectGetElementExistsByIndex = {#call Object_getElementExistsByIndex #}
libObjectGetElementValueByIndex  = {#call Object_getElementValueByIndex  #}
libObjectGetFirstIndex           = {#call Object_getFirstIndex           #}
libObjectGetLastIndex            = {#call Object_getLastIndex            #}
libObjectGetNextIndex            = {#call Object_getNextIndex            #}
libObjectGetPrevIndex            = {#call Object_getPrevIndex            #}
libObjectGetSize                 = {#call Object_getSize                 #}
libObjectRelease                 = {#call Object_release                 #}
libObjectSetAttributeValue       = {#call Object_setAttributeValue       #}
libObjectSetElementValueByIndex  = {#call Object_setElementValueByIndex  #}
libObjectTrim                    = {#call Object_trim                    #}

-- ObjectAttr
{-# INLINE libObjectAttrAddRef  #-}
{-# INLINE libObjectAttrGetInfo #-}
{-# INLINE libObjectAttrRelease #-}
libObjectAttrAddRef  = {#call ObjectAttr_addRef  #}
libObjectAttrGetInfo = {#call ObjectAttr_getInfo #}
libObjectAttrRelease = {#call ObjectAttr_release #}

-- ObjectType
{-# INLINE libObjectTypeAddRef        #-}
{-# INLINE libObjectTypeCreateObject  #-}
{-# INLINE libObjectTypeGetAttributes #-}
{-# INLINE libObjectTypeGetInfo       #-}
{-# INLINE libObjectTypeRelease       #-}
libObjectTypeAddRef        = {#call ObjectType_addRef        #}
libObjectTypeCreateObject  = {#call ObjectType_createObject  #}
libObjectTypeGetAttributes = {#call ObjectType_getAttributes #}
libObjectTypeGetInfo       = {#call ObjectType_getInfo       #}
libObjectTypeRelease       = {#call ObjectType_release       #}

-- Pool
{-# INLINE libPoolAcquireConnection     #-}
{-# INLINE libPoolAddRef                #-}
{-# INLINE libPoolClose                 #-}
{-# INLINE libPoolCreate                #-}
{-# INLINE libPoolGetBusyCount          #-}
{-# INLINE libPoolGetEncodingInfo       #-}
{-# INLINE libPoolGetGetMode            #-}
{-# INLINE libPoolGetMaxLifetimeSession #-}
{-# INLINE libPoolGetOpenCount          #-}
{-# INLINE libPoolGetStmtCacheSize      #-}
{-# INLINE libPoolGetTimeout            #-}
{-# INLINE libPoolRelease               #-}
{-# INLINE libPoolSetGetMode            #-}
{-# INLINE libPoolSetMaxLifetimeSession #-}
{-# INLINE libPoolSetStmtCacheSize      #-}
{-# INLINE libPoolSetTimeout            #-}
{-# INLINE libPoolGetWaitTimeout        #-}
{-# INLINE libPoolSetWaitTimeout        #-}
libPoolAcquireConnection     = {#call Pool_acquireConnection     #}
libPoolAddRef                = {#call Pool_addRef                #}
libPoolClose                 = {#call Pool_close                 #}
libPoolCreate                = {#call Pool_create                #}
libPoolGetBusyCount          = {#call Pool_getBusyCount          #}
libPoolGetEncodingInfo       = {#call Pool_getEncodingInfo       #}
libPoolGetGetMode            = {#call Pool_getGetMode            #}
libPoolGetMaxLifetimeSession = {#call Pool_getMaxLifetimeSession #}
libPoolGetOpenCount          = {#call Pool_getOpenCount          #}
libPoolGetStmtCacheSize      = {#call Pool_getStmtCacheSize      #}
libPoolGetTimeout            = {#call Pool_getTimeout            #}
libPoolRelease               = {#call Pool_release               #}
libPoolSetGetMode            = {#call Pool_setGetMode            #}
libPoolSetMaxLifetimeSession = {#call Pool_setMaxLifetimeSession #}
libPoolSetStmtCacheSize      = {#call Pool_setStmtCacheSize      #}
libPoolSetTimeout            = {#call Pool_setTimeout            #}
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
libPoolGetWaitTimeout        = {#call Pool_getWaitTimeout       #}
libPoolSetWaitTimeout        = {#call Pool_setWaitTimeout       #}
#else
libPoolGetWaitTimeout        = error "No implement until 2.4.0"
libPoolSetWaitTimeout        = error "No implement until 2.4.0"
#endif

-- Stmt
{-# INLINE libStmtAddRef             #-}
{-# INLINE libStmtBindByName         #-}
{-# INLINE libStmtBindByPos          #-}
{-# INLINE libStmtBindValueByName    #-}
{-# INLINE libStmtBindValueByPos     #-}
{-# INLINE libStmtClose              #-}
{-# INLINE libStmtDefine             #-}
{-# INLINE libStmtDefineValue        #-}
{-# INLINE libStmtExecute            #-}
{-# INLINE libStmtExecuteMany        #-}
{-# INLINE libStmtFetch              #-}
{-# INLINE libStmtFetchRows          #-}
{-# INLINE libStmtGetBatchErrorCount #-}
{-# INLINE libStmtGetBatchErrors     #-}
{-# INLINE libStmtGetBindCount       #-}
{-# INLINE libStmtGetBindNames       #-}
{-# INLINE libStmtGetFetchArraySize  #-}
{-# INLINE libStmtGetImplicitResult  #-}
{-# INLINE libStmtGetInfo            #-}
{-# INLINE libStmtGetNumQueryColumns #-}
{-# INLINE libStmtGetQueryInfo       #-}
{-# INLINE libStmtGetQueryValue      #-}
{-# INLINE libStmtGetRowCount        #-}
{-# INLINE libStmtGetRowCounts       #-}
{-# INLINE libStmtGetSubscrQueryId   #-}
{-# INLINE libStmtRelease            #-}
{-# INLINE libStmtScroll             #-}
{-# INLINE libStmtSetFetchArraySize  #-}
libStmtAddRef             = {#call Stmt_addRef             #}
libStmtBindByName         = {#call Stmt_bindByName         #}
libStmtBindByPos          = {#call Stmt_bindByPos          #}
libStmtBindValueByName    = {#call Stmt_bindValueByName    #}
libStmtBindValueByPos     = {#call Stmt_bindValueByPos     #}
libStmtClose              = {#call Stmt_close              #}
libStmtDefine             = {#call Stmt_define             #}
libStmtDefineValue        = {#call Stmt_defineValue        #}
libStmtExecute            = {#call Stmt_execute            #}
libStmtExecuteMany        = {#call Stmt_executeMany        #}
libStmtFetch              = {#call Stmt_fetch              #}
libStmtFetchRows          = {#call Stmt_fetchRows          #}
libStmtGetBatchErrorCount = {#call Stmt_getBatchErrorCount #}
libStmtGetBatchErrors     = {#call Stmt_getBatchErrors     #}
libStmtGetBindCount       = {#call Stmt_getBindCount       #}
libStmtGetBindNames       = {#call Stmt_getBindNames       #}
libStmtGetFetchArraySize  = {#call Stmt_getFetchArraySize  #}
libStmtGetImplicitResult  = {#call Stmt_getImplicitResult  #}
libStmtGetInfo            = {#call Stmt_getInfo            #}
libStmtGetNumQueryColumns = {#call Stmt_getNumQueryColumns #}
libStmtGetQueryInfo       = {#call Stmt_getQueryInfo       #}
libStmtGetQueryValue      = {#call Stmt_getQueryValue      #}
libStmtGetRowCount        = {#call Stmt_getRowCount        #}
libStmtGetRowCounts       = {#call Stmt_getRowCounts       #}
libStmtGetSubscrQueryId   = {#call Stmt_getSubscrQueryId   #}
libStmtRelease            = {#call Stmt_release            #}
libStmtScroll             = {#call Stmt_scroll             #}
libStmtSetFetchArraySize  = {#call Stmt_setFetchArraySize  #}

-- RowId
{-# INLINE libRowidAddRef         #-}
{-# INLINE libRowidGetStringValue #-}
{-# INLINE libRowidRelease        #-}
libRowidAddRef         = {#call Rowid_addRef         #}
libRowidGetStringValue = {#call Rowid_getStringValue #}
libRowidRelease        = {#call Rowid_release        #}

-- Subscr
{-# INLINE libSubscrAddRef      #-}
{-# INLINE libSubscrClose       #-}
{-# INLINE libSubscrPrepareStmt #-}
{-# INLINE libSubscrRelease     #-}
libSubscrAddRef      = {#call Subscr_addRef       #}
#if DPI_MAJOR_VERSION >= 3
libSubscrClose       = {#call dpiConn_unsubscribe #}
#else
libSubscrClose       = {#call Subscr_close        #}
#endif

libSubscrPrepareStmt = {#call Subscr_prepareStmt  #}
libSubscrRelease     = {#call Subscr_release      #}

-- Var
{-# INLINE libVarAddRef                #-}
{-# INLINE libVarCopyData              #-}
{-# INLINE libVarGetData               #-}
{-# INLINE libVarGetNumElementsInArray #-}
{-# INLINE libVarGetSizeInBytes        #-}
{-# INLINE libVarRelease               #-}
{-# INLINE libVarSetFromBytes          #-}
{-# INLINE libVarSetFromLob            #-}
{-# INLINE libVarSetFromObject         #-}
{-# INLINE libVarSetFromRowid          #-}
{-# INLINE libVarSetFromStmt           #-}
{-# INLINE libVarSetNumElementsInArray #-}
libVarAddRef                = {#call Var_addRef                #}
libVarCopyData              = {#call Var_copyData              #}
libVarGetData               = {#call Var_getData               #}
libVarGetNumElementsInArray = {#call Var_getNumElementsInArray #}
libVarGetSizeInBytes        = {#call Var_getSizeInBytes        #}
libVarRelease               = {#call Var_release               #}
libVarSetFromBytes          = {#call Var_setFromBytes          #}
libVarSetFromLob            = {#call Var_setFromLob            #}
libVarSetFromObject         = {#call Var_setFromObject         #}
libVarSetFromRowid          = {#call Var_setFromRowid          #}
libVarSetFromStmt           = {#call Var_setFromStmt           #}
libVarSetNumElementsInArray = {#call Var_setNumElementsInArray #}
#if DPI_MAJOR_VERSION >= 2 && DPI_MINOR_VERSION >= 4
libVarGetReturnedData       = {#call Var_getReturnedData       #}
use2_4_0                    = True
#else
libVarGetReturnedData       = error "No implement until 2.4.0"
use2_4_0                    = False
#endif

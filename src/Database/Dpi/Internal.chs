{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE DuplicateRecordFields    #-}

module Database.Dpi.Internal where

import Foreign
import Foreign.C.Types

#include <dpi.h>

{#context prefix="dpi" #}

noImplement :: a
noImplement = error "Not supported yet"

te :: (Integral n, Enum e) => n -> e
te = toEnum . fromIntegral

-- Enum
{#enum AuthMode            as ^ {underscoreToCase} deriving (Show) #}
{#enum ConnCloseMode       as ^ {underscoreToCase} deriving (Show) #}
{#enum CreateMode          as ^ {underscoreToCase} deriving (Show) #}
{#enum DeqMode             as ^ {underscoreToCase} deriving (Show) #}
{#enum DeqNavigation       as ^ {underscoreToCase} deriving (Show) #}
{#enum EventType           as ^ {underscoreToCase} deriving (Show) #}
{#enum ExecMode            as ^ {underscoreToCase} deriving (Show) #}
{#enum FetchMode           as ^ {underscoreToCase} deriving (Show) #}
{#enum MessageDeliveryMode as ^ {underscoreToCase} deriving (Show) #}
{#enum MessageState        as ^ {underscoreToCase} deriving (Show) #}
{#enum NativeTypeNum       as ^ {underscoreToCase} deriving (Show) #}
{#enum OpCode              as ^ {underscoreToCase} deriving (Show) #}
{#enum OracleTypeNum       as ^ {underscoreToCase} deriving (Show) #}
{#enum PoolCloseMode       as ^ {underscoreToCase} deriving (Show) #}
{#enum PoolGetMode         as ^ {underscoreToCase} deriving (Show) #}
{#enum Purity              as ^ {underscoreToCase} deriving (Show) #}
{#enum ShutdownMode        as ^ {underscoreToCase} deriving (Show) #}
{#enum StartupMode         as ^ {underscoreToCase} deriving (Show) #}
{#enum StatementType       as ^ {underscoreToCase} deriving (Show) #}
{#enum SubscrNamespace     as ^ {underscoreToCase} deriving (Show) #}
{#enum SubscrProtocol      as ^ {underscoreToCase} deriving (Show) #}
{#enum SubscrQOS           as ^ {underscoreToCase} deriving (Show) #}
{#enum Visibility          as ^ {underscoreToCase} deriving (Show) #}

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

--        Inner               Data
{#pointer *Bytes      as Ptr_Bytes      -> Data_Bytes      #}
{#pointer *IntervalDS as Ptr_IntervalDS -> Data_IntervalDS #}
{#pointer *IntervalYM as Ptr_IntervalYM -> Data_IntervalYM #}
{#pointer *Timestamp  as Ptr_Timestamp  -> Data_Timestamp  #}

data Data_Bytes = Data_Bytes
  { ptr      :: Ptr CChar
  , length   :: CUInt
  , encoding :: Ptr CChar
  } deriving Show

instance Storable Data_Bytes where
  sizeOf    _ = {#sizeof  Bytes#}
  alignment _ = {#alignof Bytes#}
  poke      _ = noImplement
  peek      p = do
    ptr      <- {#get Bytes -> ptr      #} p
    length   <- {#get Bytes -> length   #} p
    encoding <- {#get Bytes -> encoding #} p
    return Data_Bytes {..}

data Data_IntervalDS = Data_IntervalDS
  { days     :: CInt
  , hours    :: CInt
  , minutes  :: CInt
  , seconds  :: CInt
  , fseconds :: CInt
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
  { years    :: CInt
  , months   :: CInt
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
  { year           :: CShort
  , month          :: CUChar
  , day            :: CUChar
  , hour           :: CUChar
  , minute         :: CUChar
  , second         :: CUChar
  , fsecond        :: CUInt
  , tzHourOffset   :: CSChar
  , tzMinuteOffset :: CSChar
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
{#pointer *Data               as PtrNullableDataBuffer -> NullableDataBuffer      #}
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
  { namespaceName       :: Ptr CChar
  , namespaceNameLength :: CUInt
  , name                :: Ptr CChar
  , nameLength          :: CUInt
  , value               :: Ptr CChar
  , valueLength         :: CUInt
  } deriving Show

instance Storable Data_AppContext where
  sizeOf    _ = {#sizeof  AppContext #}
  alignment _ = {#alignof AppContext #}
  poke      _ = noImplement
  peek      p = do
    namespaceName       <- {#get AppContext -> namespaceName       #} p
    namespaceNameLength <- {#get AppContext -> namespaceNameLength #} p
    name                <- {#get AppContext -> name                #} p
    nameLength          <- {#get AppContext -> nameLength          #} p
    value               <- {#get AppContext -> value               #} p
    valueLength         <- {#get AppContext -> valueLength         #} p
    return Data_AppContext {..}

data Data_CommonCreateParams  = Data_CommonCreateParams
  { createMode       :: CreateMode
  , encoding         :: Ptr CChar
  , nencoding        :: Ptr CChar
  , edition          :: Ptr CChar
  , editionLength    :: CUInt
  , driverName       :: Ptr CChar
  , driverNameLength :: CUInt
  } deriving Show

instance Storable Data_CommonCreateParams where
  sizeOf    _ = {#sizeof  CommonCreateParams #}
  alignment _ = {#alignof CommonCreateParams #}
  poke      _ = noImplement
  peek      p = do
    createMode       <- te <$> {#get CommonCreateParams -> createMode       #} p
    encoding         <- {#get CommonCreateParams -> encoding         #} p
    nencoding        <- {#get CommonCreateParams -> nencoding        #} p
    edition          <- {#get CommonCreateParams -> edition          #} p
    editionLength    <- {#get CommonCreateParams -> editionLength    #} p
    driverName       <- {#get CommonCreateParams -> driverName       #} p
    driverNameLength <- {#get CommonCreateParams -> driverNameLength #} p
    return Data_CommonCreateParams {..}

data Data_ConnCreateParams  = Data_ConnCreateParams
  { authMode                   :: AuthMode
  , connectionClass            :: Ptr CChar
  , connectionClassLength      :: CUInt
  , purity                     :: Purity
  , newPassword                :: Ptr CChar
  , newPasswordLength          :: CUInt
  , appContext                 :: PtrAppContext
  , numAppContext              :: CUInt
  , externalAuth               :: CInt
  , externalHandle             :: Ptr ()
  , pool                       :: Ptr DPI_Pool
  , tag                        :: Ptr CChar
  , tagLength                  :: CUInt
  , matchAnyTag                :: CInt
  , outTag                     :: Ptr CChar
  , outTagLength               :: CUInt
  , outTagFound                :: CInt
  , shardingKeyColumns         :: PtrShardingKeyColumn
  , numShardingKeyColumns      :: CUChar
  , superShardingKeyColumns    :: PtrShardingKeyColumn
  , numSuperShardingKeyColumns :: CUChar
  } deriving Show

instance Storable Data_ConnCreateParams where
  sizeOf    _ = {#sizeof  ConnCreateParams #}
  alignment _ = {#alignof ConnCreateParams #}
  poke      _ = noImplement
  peek      p = do
    authMode                   <- te <$> {#get ConnCreateParams -> authMode                   #} p
    connectionClass            <- {#get ConnCreateParams -> connectionClass            #} p
    connectionClassLength      <- {#get ConnCreateParams -> connectionClassLength      #} p
    purity                     <- te <$> {#get ConnCreateParams -> purity                     #} p
    newPassword                <- {#get ConnCreateParams -> newPassword                #} p
    newPasswordLength          <- {#get ConnCreateParams -> newPasswordLength          #} p
    appContext                 <- {#get ConnCreateParams -> appContext                 #} p
    numAppContext              <- {#get ConnCreateParams -> numAppContext              #} p
    externalAuth               <- {#get ConnCreateParams -> externalAuth               #} p
    externalHandle             <- {#get ConnCreateParams -> externalHandle             #} p
    pool                       <- {#get ConnCreateParams -> pool                       #} p
    tag                        <- {#get ConnCreateParams -> tag                        #} p
    tagLength                  <- {#get ConnCreateParams -> tagLength                  #} p
    matchAnyTag                <- {#get ConnCreateParams -> matchAnyTag                #} p
    outTag                     <- {#get ConnCreateParams -> outTag                     #} p
    outTagLength               <- {#get ConnCreateParams -> outTagLength               #} p
    outTagFound                <- {#get ConnCreateParams -> outTagFound                #} p
    shardingKeyColumns         <- {#get ConnCreateParams -> shardingKeyColumns         #} p
    numShardingKeyColumns      <- {#get ConnCreateParams -> numShardingKeyColumns      #} p
    superShardingKeyColumns    <- {#get ConnCreateParams -> superShardingKeyColumns    #} p
    numSuperShardingKeyColumns <- {#get ConnCreateParams -> numSuperShardingKeyColumns #} p
    return Data_ConnCreateParams {..}

data DataValue
  = DataNull
  | DataInt64      CLLong
  | DataUint64     CULLong
  | DataFloat      CFloat
  | DataDouble     CDouble
  | DataBytes      Ptr_Bytes
  | DataTimestamp  Ptr_Timestamp
  | DataIntervalDs Ptr_IntervalDS
  | DataIntervalYm Ptr_IntervalYM
  | DataLob        (Ptr DPI_Lob)
  | DataObject     (Ptr DPI_Object)
  | DataStmt       (Ptr DPI_Stmt)
  | DataBoolean    Bool
  | DataRowid      (Ptr DPI_Rowid)

newtype DataBuffer = DataBuffer (NativeTypeNum -> IO DataValue)

instance Storable DataBuffer where
  sizeOf    _ = {#sizeof  DataBuffer #}
  alignment _ = {#alignof DataBuffer #}
  poke      _ = noImplement
  peek      p = return $ DataBuffer $ go p
    where
      go p NativeTypeInt64      = DataInt64       <$>       {#get DataBuffer ->         asInt64  #}           p
      go p NativeTypeUint64     = DataUint64      <$>       {#get DataBuffer ->         asUint64 #}           p
      go p NativeTypeFloat      = DataFloat       <$>       {#get DataBuffer ->         asFloat  #}           p
      go p NativeTypeDouble     = DataDouble      <$>       {#get DataBuffer ->         asDouble #}           p
      go p NativeTypeBytes      = (DataBytes      .castPtr) <$>   {#get      DataBuffer ->       asBytes      #} p
      go p NativeTypeTimestamp  = (DataTimestamp  .castPtr) <$>   {#get      DataBuffer ->       asTimestamp  #} p
      go p NativeTypeIntervalDs = (DataIntervalDs .castPtr) <$>   {#get      DataBuffer ->       asIntervalDS #} p
      go p NativeTypeIntervalYm = (DataIntervalYm .castPtr) <$>   {#get      DataBuffer ->       asIntervalYM #} p
      go p NativeTypeLob        = DataLob         <$>       {#get DataBuffer ->         asLOB    #}           p
      go p NativeTypeObject     = DataObject      <$>       {#get DataBuffer ->         asObject #}           p
      go p NativeTypeStmt       = DataStmt        <$>       {#get DataBuffer ->         asStmt   #}           p
      go p NativeTypeBoolean    = (DataBoolean    .toBool)  <$>   {#get      DataBuffer ->       asBoolean    #} p
      go p NativeTypeRowid      = DataRowid       <$>       {#get DataBuffer ->         asRowid  #}           p
      -- go _ _                    = noImplement

newtype NullableDataBuffer = NullableDataBuffer (NativeTypeNum -> IO DataValue)

instance Storable NullableDataBuffer where
  sizeOf    _ = {#sizeof  Data #}
  alignment _ = {#alignof Data #}
  poke      _ = noImplement
  peek      p = return $ NullableDataBuffer $ go p
    where
      go p t = do
        isNull <- toBool <$> {#get Data -> isNull #} p
        if isNull 
          then return DataNull
          else do
            p'           <- castPtr <$> {#get Data -> value #} p
            DataBuffer v <- peek p'
            v t

data Data_DataTypeInfo  = Data_DataTypeInfo
  { oracleTypeNum        :: OracleTypeNum
  , defaultNativeTypeNum :: NativeTypeNum
  , ociTypeCode          :: CUShort
  , dbSizeInBytes        :: CUInt
  , clientSizeInBytes    :: CUInt
  , sizeInChars          :: CUInt
  , precision            :: CShort
  , scale                :: CSChar
  , fsPrecision          :: CUChar
  , objectType           :: Ptr DPI_ObjectType
  } deriving Show

instance Storable Data_DataTypeInfo where
  sizeOf    _ = {#sizeof  DataTypeInfo #}
  alignment _ = {#alignof DataTypeInfo #}
  poke      _ = noImplement
  peek      p = do
    oracleTypeNum        <- te <$> {#get DataTypeInfo -> oracleTypeNum        #} p
    defaultNativeTypeNum <- te <$> {#get DataTypeInfo -> defaultNativeTypeNum #} p
    ociTypeCode          <- {#get DataTypeInfo -> ociTypeCode          #} p
    dbSizeInBytes        <- {#get DataTypeInfo -> dbSizeInBytes        #} p
    clientSizeInBytes    <- {#get DataTypeInfo -> clientSizeInBytes    #} p
    sizeInChars          <- {#get DataTypeInfo -> sizeInChars          #} p
    precision            <- {#get DataTypeInfo -> precision            #} p
    scale                <- {#get DataTypeInfo -> scale                #} p
    fsPrecision          <- {#get DataTypeInfo -> fsPrecision          #} p
    objectType           <- {#get DataTypeInfo -> objectType           #} p
    return Data_DataTypeInfo {..}


data Data_EncodingInfo  = Data_EncodingInfo
  { encoding              :: Ptr CChar
  , maxBytesPerCharacter  :: CInt
  , nencoding             :: Ptr CChar
  , nmaxBytesPerCharacter :: CInt
  } deriving Show

instance Storable Data_EncodingInfo where
  sizeOf    _ = {#sizeof  EncodingInfo #}
  alignment _ = {#alignof EncodingInfo #}
  poke      _ = noImplement
  peek      p = do
    encoding              <- {#get EncodingInfo -> encoding              #} p
    maxBytesPerCharacter  <- {#get EncodingInfo -> maxBytesPerCharacter  #} p
    nencoding             <- {#get EncodingInfo -> nencoding             #} p
    nmaxBytesPerCharacter <- {#get EncodingInfo -> nmaxBytesPerCharacter #} p
    return Data_EncodingInfo {..}

data Data_ErrorInfo  = Data_ErrorInfo
  { code          :: CInt
  , offset        :: CUShort
  , message       :: Ptr CChar
  , messageLength :: CUInt
  , encoding      :: Ptr CChar
  , fnName        :: Ptr CChar
  , action        :: Ptr CChar
  , sqlState      :: Ptr CChar
  , isRecoverable :: Bool
  } deriving Show

instance Storable Data_ErrorInfo where
  sizeOf    _ = {#sizeof  ErrorInfo #}
  alignment _ = {#alignof ErrorInfo #}
  poke      _ = noImplement
  peek      p = do
    code          <- {#get ErrorInfo -> code          #} p
    offset        <- {#get ErrorInfo -> offset        #} p
    message       <- {#get ErrorInfo -> message       #} p
    messageLength <- {#get ErrorInfo -> messageLength #} p
    encoding      <- {#get ErrorInfo -> encoding      #} p
    fnName        <- {#get ErrorInfo -> fnName        #} p
    action        <- {#get ErrorInfo -> action        #} p
    sqlState      <- {#get ErrorInfo -> sqlState      #} p
    isRecoverable <- toBool <$> {#get ErrorInfo -> isRecoverable #} p
    return Data_ErrorInfo {..}

data Data_ObjectAttrInfo  = Data_ObjectAttrInfo
  { name       :: Ptr CChar
  , nameLength :: CUInt
  , typeInfo   :: PtrDataTypeInfo
  } deriving Show

instance Storable Data_ObjectAttrInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke      _ = noImplement
  peek      p = do
    name       <- {#get ObjectAttrInfo -> name       #} p
    nameLength <- {#get ObjectAttrInfo -> nameLength #} p
    typeInfo   <- castPtr <$> {#get ObjectAttrInfo -> typeInfo   #} p
    return Data_ObjectAttrInfo {..}

data Data_ObjectTypeInfo  = Data_ObjectTypeInfo
  { schema          :: Ptr CChar
  , schemaLength    :: CUInt
  , name            :: Ptr CChar
  , nameLength      :: CUInt
  , isCollection    :: Bool
  , elementTypeInfo :: PtrDataTypeInfo
  , numAttributes   :: CUShort
  } deriving Show

instance Storable Data_ObjectTypeInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke      _ = noImplement
  peek      p = do
    schema          <- {#get ObjectTypeInfo -> schema          #} p
    schemaLength    <- {#get ObjectTypeInfo -> schemaLength    #} p
    name            <- {#get ObjectTypeInfo -> name            #} p
    nameLength      <- {#get ObjectTypeInfo -> nameLength      #} p
    isCollection    <- toBool  <$> {#get ObjectTypeInfo -> isCollection    #} p
    elementTypeInfo <- castPtr <$> {#get ObjectTypeInfo -> elementTypeInfo #} p
    numAttributes   <- {#get ObjectTypeInfo -> numAttributes   #} p
    return Data_ObjectTypeInfo {..}

data Data_PoolCreateParams  = Data_PoolCreateParams
  { minSessions       :: CUInt
  , maxSessions       :: CUInt
  , sessionIncrement  :: CUInt
  , pingInterval      :: CInt
  , pingTimeout       :: CInt
  , homogeneous       :: CInt
  , externalAuth      :: CInt
  , getMode           :: PoolGetMode
  , outPoolName       :: Ptr CChar
  , outPoolNameLength :: CUInt
  } deriving Show

instance Storable Data_PoolCreateParams where
  sizeOf    _ = {#sizeof  PoolCreateParams #}
  alignment _ = {#alignof PoolCreateParams #}
  poke      _ = noImplement
  peek      p = do
    minSessions       <- {#get PoolCreateParams -> minSessions       #} p
    maxSessions       <- {#get PoolCreateParams -> maxSessions       #} p
    sessionIncrement  <- {#get PoolCreateParams -> sessionIncrement  #} p
    pingInterval      <- {#get PoolCreateParams -> pingInterval      #} p
    pingTimeout       <- {#get PoolCreateParams -> pingTimeout       #} p
    homogeneous       <- {#get PoolCreateParams -> homogeneous       #} p
    externalAuth      <- {#get PoolCreateParams -> externalAuth      #} p
    getMode           <- te <$> {#get PoolCreateParams -> getMode           #} p
    outPoolName       <- {#get PoolCreateParams -> outPoolName       #} p
    outPoolNameLength <- {#get PoolCreateParams -> outPoolNameLength #} p
    return Data_PoolCreateParams {..}

data Data_QueryInfo = Data_QueryInfo
  { name       :: Ptr CChar
  , nameLength :: CUInt
  , typeInfo   :: PtrDataTypeInfo
  , nullOk     :: Bool
  } deriving Show

instance Storable Data_QueryInfo where
  sizeOf    _ = {#sizeof  QueryInfo #}
  alignment _ = {#alignof QueryInfo #}
  poke      _ = noImplement
  peek      p = do
    name       <- {#get QueryInfo -> name       #} p
    nameLength <- {#get QueryInfo -> nameLength #} p
    typeInfo   <- castPtr <$> {#get QueryInfo -> typeInfo   #} p
    nullOk     <- toBool  <$> {#get QueryInfo -> nullOk     #} p
    return Data_QueryInfo {..}

data Data_ShardingKeyColumn  = Data_ShardingKeyColumn
  { oracleTypeNum :: OracleTypeNum
  , nativeTypeNum :: NativeTypeNum
  -- , value         :: DataBuffer
  } deriving Show

instance Storable Data_ShardingKeyColumn where
  sizeOf    _ = {#sizeof  ShardingKeyColumn #}
  alignment _ = {#alignof ShardingKeyColumn #}
  poke      _ = noImplement
  peek      p = do
    oracleTypeNum <- te      <$> {#get ShardingKeyColumn -> oracleTypeNum #} p
    nativeTypeNum <- te      <$> {#get ShardingKeyColumn -> nativeTypeNum #} p
    value         <- castPtr <$> {#get ShardingKeyColumn -> value         #} p
    return Data_ShardingKeyColumn {..}

data Data_StmtInfo = Data_StmtInfo
  { isQuery       :: Bool
  , isPLSQL       :: Bool
  , isDDL         :: Bool
  , isDML         :: Bool
  , statementType :: StatementType
  , isReturning   :: Bool
  } deriving Show

instance Storable Data_StmtInfo where
  sizeOf    _ = {#sizeof  StmtInfo #}
  alignment _ = {#alignof StmtInfo #}
  poke      _ = noImplement
  peek      p = do
    isQuery       <- toBool <$> {#get StmtInfo -> isQuery       #} p
    isPLSQL       <- toBool <$> {#get StmtInfo -> isPLSQL       #} p
    isDDL         <- toBool <$> {#get StmtInfo -> isDDL         #} p
    isDML         <- toBool <$> {#get StmtInfo -> isDML         #} p
    statementType <- te     <$> {#get StmtInfo -> statementType #} p
    isReturning   <- toBool <$> {#get StmtInfo -> isReturning   #} p
    return Data_StmtInfo {..}

data Data_SubscrCreateParams = Data_SubscrCreateParams
  { subscrNamespace     :: SubscrNamespace
  , protocol            :: SubscrProtocol
  , qos                 :: SubscrQOS
  , operations          :: CInt
  , portNumber          :: CUInt
  , timeout             :: CUInt
  , name                :: Ptr CChar
  , nameLength          :: CUInt
  , callback            :: FunPtr (Ptr () -> PtrSubscrMessage -> IO ())
  , callbackContext     :: Ptr ()
  , recipientName       :: Ptr CChar
  , recipientNameLength :: CUInt
  } deriving Show

instance Storable Data_SubscrCreateParams where
  sizeOf    _ = {#sizeof  SubscrCreateParams #}
  alignment _ = {#alignof SubscrCreateParams #}
  poke      _ = noImplement
  peek      p = do
    subscrNamespace     <- te    <$>                {#get SubscrCreateParams  -> subscrNamespace #} p
    protocol            <- te    <$>                {#get SubscrCreateParams  -> protocol        #} p
    qos                 <- te    <$>                {#get SubscrCreateParams  -> qos             #} p
    operations          <- {#get SubscrCreateParams ->    operations          #} p
    portNumber          <- {#get SubscrCreateParams ->    portNumber          #} p
    timeout             <- {#get SubscrCreateParams ->    timeout             #} p
    name                <- {#get SubscrCreateParams ->    name                #} p
    nameLength          <- {#get SubscrCreateParams ->    nameLength          #} p
    callback            <- {#get SubscrCreateParams ->    callback            #} p
    callbackContext     <- {#get SubscrCreateParams ->    callbackContext     #} p
    recipientName       <- {#get SubscrCreateParams ->    recipientName       #} p
    recipientNameLength <- {#get SubscrCreateParams ->    recipientNameLength #} p
    return Data_SubscrCreateParams {..}

data Data_SubscrMessage = Data_SubscrMessage
  { eventType    :: EventType
  , dbName       :: Ptr CChar
  , dbNameLength :: CUInt
  , tables       :: PtrSubscrMessageTable
  , numTables    :: CUInt
  , queries      :: PtrSubscrMessageQuery
  , numQueries   :: CUInt
  , errorInfo    :: PtrErrorInfo
  , txId         :: Ptr ()
  , txIdLength   :: CUInt
  } deriving Show

instance Storable Data_SubscrMessage where
  sizeOf    _ = {#sizeof  SubscrMessage #}
  alignment _ = {#alignof SubscrMessage #}
  poke      _ = noImplement
  peek      p = do
    eventType    <- te <$> {#get SubscrMessage -> eventType    #} p
    dbName       <- {#get SubscrMessage -> dbName       #} p
    dbNameLength <- {#get SubscrMessage -> dbNameLength #} p
    tables       <- {#get SubscrMessage -> tables       #} p
    numTables    <- {#get SubscrMessage -> numTables    #} p
    queries      <- {#get SubscrMessage -> queries      #} p
    numQueries   <- {#get SubscrMessage -> numQueries   #} p
    errorInfo    <- {#get SubscrMessage -> errorInfo    #} p
    txId         <- {#get SubscrMessage -> txId         #} p
    txIdLength   <- {#get SubscrMessage -> txIdLength   #} p
    return Data_SubscrMessage {..}

data Data_SubscrMessageQuery = Data_SubscrMessageQuery
  { id        :: CULLong
  , operation :: OpCode
  , tables    :: PtrSubscrMessageTable
  , numTables :: CUInt
  } deriving Show

instance Storable Data_SubscrMessageQuery where
  sizeOf    _ = {#sizeof  SubscrMessageQuery #}
  alignment _ = {#alignof SubscrMessageQuery #}
  poke      _ = noImplement
  peek      p = do
    id        <- {#get SubscrMessageQuery -> id        #} p
    operation <- te <$> {#get SubscrMessageQuery -> operation #} p
    tables    <- {#get SubscrMessageQuery -> tables    #} p
    numTables <- {#get SubscrMessageQuery -> numTables #} p
    return Data_SubscrMessageQuery {..}

data Data_SubscrMessageRow = Data_SubscrMessageRow
  { operation   :: OpCode
  , rowid       :: Ptr CChar
  , rowidLength :: CUInt
  } deriving Show

instance Storable Data_SubscrMessageRow where
  sizeOf    _ = {#sizeof  SubscrMessageRow #}
  alignment _ = {#alignof SubscrMessageRow #}
  poke      _ = noImplement
  peek      p = do
    operation   <- te <$> {#get SubscrMessageRow -> operation  #} p
    rowid       <- {#get SubscrMessageRow -> rowid       #} p
    rowidLength <- {#get SubscrMessageRow -> rowidLength #} p
    return Data_SubscrMessageRow {..}

data Data_SubscrMessageTable = Data_SubscrMessageTable
  { operation  :: OpCode
  , name       :: Ptr CChar
  , nameLength :: CUInt
  , rows       :: PtrSubscrMessageRow
  , numRows    :: CUInt
  } deriving Show

instance Storable Data_SubscrMessageTable where
  sizeOf    _ = {#sizeof  SubscrMessageTable #}
  alignment _ = {#alignof SubscrMessageTable #}
  poke      _ = noImplement
  peek      p = do
    operation  <- te <$> {#get SubscrMessageTable -> operation  #} p
    name       <- {#get SubscrMessageTable -> name       #} p
    nameLength <- {#get SubscrMessageTable -> nameLength #} p
    rows       <- {#get SubscrMessageTable -> rows       #} p
    numRows    <- {#get SubscrMessageTable -> numRows    #} p
    return Data_SubscrMessageTable {..}

data Data_VersionInfo = Data_VersionInfo
  { versionNum     :: CInt
  , releaseNum     :: CInt
  , updateNum      :: CInt
  , portReleaseNum :: CInt
  , portUpdateNum  :: CInt
  , fullVersionNum :: CUInt
  } deriving Show

instance Storable Data_VersionInfo where
  sizeOf    _ = {#sizeof  VersionInfo #}
  alignment _ = {#alignof VersionInfo #}
  poke      _ = noImplement
  peek      p = do
    versionNum     <- {#get VersionInfo -> versionNum     #} p
    releaseNum     <- {#get VersionInfo -> releaseNum     #} p
    updateNum      <- {#get VersionInfo -> updateNum      #} p
    portReleaseNum <- {#get VersionInfo -> portReleaseNum #} p
    portUpdateNum  <- {#get VersionInfo -> portUpdateNum  #} p
    fullVersionNum <- {#get VersionInfo -> fullVersionNum #} p
    return Data_VersionInfo {..}

-- Context 
contextCreate                 = {#call Context_create                 #}
contextDestroy                = {#call Context_destroy                #}
contextGetClientVersion       = {#call Context_getClientVersion       #}
contextInitCommonCreateParams = {#call Context_initCommonCreateParams #}
contextInitConnCreateParams   = {#call Context_initConnCreateParams   #}
contextInitPoolCreateParams   = {#call Context_initPoolCreateParams   #}
contextInitSubscrCreateParams = {#call Context_initSubscrCreateParams #}
contextGetError               = {#call Context_getError               #}

-- Conn

connAddRef              = {#call Conn_addRef              #}
connBeginDistribTrans   = {#call Conn_beginDistribTrans   #}
connBreakExecution      = {#call Conn_breakExecution      #}
connChangePassword      = {#call Conn_changePassword      #}
connClose               = {#call Conn_close               #}
connCommit              = {#call Conn_commit              #}
connCreate              = {#call Conn_create              #}
connDeqObject           = {#call Conn_deqObject           #}
connEnqObject           = {#call Conn_enqObject           #}
connGetCurrentSchema    = {#call Conn_getCurrentSchema    #}
connGetEdition          = {#call Conn_getEdition          #}
connGetEncodingInfo     = {#call Conn_getEncodingInfo     #}
connGetExternalName     = {#call Conn_getExternalName     #}
connGetHandle           = {#call Conn_getHandle           #}
connGetInternalName     = {#call Conn_getInternalName     #}
connGetLTXID            = {#call Conn_getLTXID            #}
connGetObjectType       = {#call Conn_getObjectType       #}
connGetServerVersion    = {#call Conn_getServerVersion    #}
connGetStmtCacheSize    = {#call Conn_getStmtCacheSize    #}
connNewDeqOptions       = {#call Conn_newDeqOptions       #}
connNewEnqOptions       = {#call Conn_newEnqOptions       #}
connNewMsgProps         = {#call Conn_newMsgProps         #}
connNewSubscription     = {#call Conn_newSubscription     #}
connNewTempLob          = {#call Conn_newTempLob          #}
connNewVar              = {#call Conn_newVar              #}
connPing                = {#call Conn_ping                #}
connPrepareDistribTrans = {#call Conn_prepareDistribTrans #}
connPrepareStmt         = {#call Conn_prepareStmt         #}
connRelease             = {#call Conn_release             #}
connRollback            = {#call Conn_rollback            #}
connSetAction           = {#call Conn_setAction           #}
connSetClientIdentifier = {#call Conn_setClientIdentifier #}
connSetClientInfo       = {#call Conn_setClientInfo       #}
connSetCurrentSchema    = {#call Conn_setCurrentSchema    #}
connSetDbOp             = {#call Conn_setDbOp             #}
connSetExternalName     = {#call Conn_setExternalName     #}
connSetInternalName     = {#call Conn_setInternalName     #}
connSetModule           = {#call Conn_setModule           #}
connSetStmtCacheSize    = {#call Conn_setStmtCacheSize    #}
connShutdownDatabase    = {#call Conn_shutdownDatabase    #}
connStartupDatabase     = {#call Conn_startupDatabase     #}

-- Data 
dataGetDouble     = {#call Data_getDouble     #}
dataGetBytes      = {#call Data_getBytes      #}
dataGetIntervalDS = {#call Data_getIntervalDS #}
dataGetIntervalYM = {#call Data_getIntervalYM #}
dataGetLOB        = {#call Data_getLOB        #}
dataGetObject     = {#call Data_getObject     #}
dataGetStmt       = {#call Data_getStmt       #}
dataGetTimestamp  = {#call Data_getTimestamp  #}
dataGetFloat      = {#call Data_getFloat      #}
dataGetBool       = {#call Data_getBool       #}
dataGetInt64      = {#call Data_getInt64      #}
dataGetUint64     = {#call Data_getUint64     #}
dataSetBool       = {#call Data_setBool       #}
dataSetBytes      = {#call Data_setBytes      #}
dataSetDouble     = {#call Data_setDouble     #}
dataSetFloat      = {#call Data_setFloat      #}
dataSetInt64      = {#call Data_setInt64      #}
dataSetIntervalDS = {#call Data_setIntervalDS #}
dataSetIntervalYM = {#call Data_setIntervalYM #}
dataSetLOB        = {#call Data_setLOB        #}
dataSetObject     = {#call Data_setObject     #}
dataSetStmt       = {#call Data_setStmt       #}
dataSetTimestamp  = {#call Data_setTimestamp  #}
dataSetUint64     = {#call Data_setUint64     #}


-- DeqOptions
deqOptionsAddRef            = {#call DeqOptions_addRef            #}
deqOptionsGetCondition      = {#call DeqOptions_getCondition      #}
deqOptionsGetConsumerName   = {#call DeqOptions_getConsumerName   #}
deqOptionsGetCorrelation    = {#call DeqOptions_getCorrelation    #}
deqOptionsGetMode           = {#call DeqOptions_getMode           #}
deqOptionsGetMsgId          = {#call DeqOptions_getMsgId          #}
deqOptionsGetNavigation     = {#call DeqOptions_getNavigation     #}
deqOptionsGetTransformation = {#call DeqOptions_getTransformation #}
deqOptionsGetVisibility     = {#call DeqOptions_getVisibility     #}
deqOptionsGetWait           = {#call DeqOptions_getWait           #}
deqOptionsRelease           = {#call DeqOptions_release           #}
deqOptionsSetCondition      = {#call DeqOptions_setCondition      #}
deqOptionsSetConsumerName   = {#call DeqOptions_setConsumerName   #}
deqOptionsSetCorrelation    = {#call DeqOptions_setCorrelation    #}
deqOptionsSetDeliveryMode   = {#call DeqOptions_setDeliveryMode   #}
deqOptionsSetMode           = {#call DeqOptions_setMode           #}
deqOptionsSetMsgId          = {#call DeqOptions_setMsgId          #}
deqOptionsSetNavigation     = {#call DeqOptions_setNavigation     #}
deqOptionsSetTransformation = {#call DeqOptions_setTransformation #}
deqOptionsSetVisibility     = {#call DeqOptions_setVisibility     #}
deqOptionsSetWait           = {#call DeqOptions_setWait           #}

-- EnqOptions
enqOptionsAddRef            = {#call EnqOptions_addRef            #}
enqOptionsGetTransformation = {#call EnqOptions_getTransformation #}
enqOptionsGetVisibility     = {#call EnqOptions_getVisibility     #}
enqOptionsRelease           = {#call EnqOptions_release           #}
enqOptionsSetDeliveryMode   = {#call EnqOptions_setDeliveryMode   #}
enqOptionsSetTransformation = {#call EnqOptions_setTransformation #}
enqOptionsSetVisibility     = {#call EnqOptions_setVisibility     #}

-- Lob
lobAddRef                  = {#call Lob_addRef                  #}
lobClose                   = {#call Lob_close                   #}
lobCloseResource           = {#call Lob_closeResource           #}
lobCopy                    = {#call Lob_copy                    #}
lobFlushBuffer             = {#call Lob_flushBuffer             #}
lobGetBufferSize           = {#call Lob_getBufferSize           #}
lobGetChunkSize            = {#call Lob_getChunkSize            #}
lobGetDirectoryAndFileName = {#call Lob_getDirectoryAndFileName #}
lobGetFileExists           = {#call Lob_getFileExists           #}
lobGetIsResourceOpen       = {#call Lob_getIsResourceOpen       #}
lobGetSize                 = {#call Lob_getSize                 #}
lobOpenResource            = {#call Lob_openResource            #}
lobReadBytes               = {#call Lob_readBytes               #}
lobRelease                 = {#call Lob_release                 #}
lobSetDirectoryAndFileName = {#call Lob_setDirectoryAndFileName #}
lobSetFromBytes            = {#call Lob_setFromBytes            #}
lobTrim                    = {#call Lob_trim                    #}
lobWriteBytes              = {#call Lob_writeBytes              #}

-- MsgProps
msgPropsAddRef           = {#call MsgProps_addRef           #}
msgPropsGetCorrelation   = {#call MsgProps_getCorrelation   #}
msgPropsGetDelay         = {#call MsgProps_getDelay         #}
msgPropsGetDeliveryMode  = {#call MsgProps_getDeliveryMode  #}
msgPropsGetEnqTime       = {#call MsgProps_getEnqTime       #}
msgPropsGetExceptionQ    = {#call MsgProps_getExceptionQ    #}
msgPropsGetExpiration    = {#call MsgProps_getExpiration    #}
msgPropsGetNumAttempts   = {#call MsgProps_getNumAttempts   #}
msgPropsGetOriginalMsgId = {#call MsgProps_getOriginalMsgId #}
msgPropsGetPriority      = {#call MsgProps_getPriority      #}
msgPropsGetState         = {#call MsgProps_getState         #}
msgPropsRelease          = {#call MsgProps_release          #}
msgPropsSetCorrelation   = {#call MsgProps_setCorrelation   #}
msgPropsSetDelay         = {#call MsgProps_setDelay         #}
msgPropsSetExceptionQ    = {#call MsgProps_setExceptionQ    #}
msgPropsSetExpiration    = {#call MsgProps_setExpiration    #}
msgPropsSetOriginalMsgId = {#call MsgProps_setOriginalMsgId #}
msgPropsSetPriority      = {#call MsgProps_setPriority      #}

-- Object
objectAddRef                  = {#call Object_addRef                  #}
objectAppendElement           = {#call Object_appendElement           #}
objectCopy                    = {#call Object_copy                    #}
objectDeleteElementByIndex    = {#call Object_deleteElementByIndex    #}
objectGetAttributeValue       = {#call Object_getAttributeValue       #}
objectGetElementExistsByIndex = {#call Object_getElementExistsByIndex #}
objectGetElementValueByIndex  = {#call Object_getElementValueByIndex  #}
objectGetFirstIndex           = {#call Object_getFirstIndex           #}
objectGetLastIndex            = {#call Object_getLastIndex            #}
objectGetNextIndex            = {#call Object_getNextIndex            #}
objectGetPrevIndex            = {#call Object_getPrevIndex            #}
objectGetSize                 = {#call Object_getSize                 #}
objectRelease                 = {#call Object_release                 #}
objectSetAttributeValue       = {#call Object_setAttributeValue       #}
objectSetElementValueByIndex  = {#call Object_setElementValueByIndex  #}
objectTrim                    = {#call Object_trim                    #}

-- ObjectAttr
objectAttrAddRef  = {#call ObjectAttr_addRef  #}
objectAttrGetInfo = {#call ObjectAttr_getInfo #}
objectAttrRelease = {#call ObjectAttr_release #}

-- ObjectType
objectTypeAddRef        = {#call ObjectType_addRef        #}
objectTypeCreateObject  = {#call ObjectType_createObject  #}
objectTypeGetAttributes = {#call ObjectType_getAttributes #}
objectTypeGetInfo       = {#call ObjectType_getInfo       #}
objectTypeRelease       = {#call ObjectType_release       #}

-- Pool
poolAcquireConnection     = {#call Pool_acquireConnection     #}
poolAddRef                = {#call Pool_addRef                #}
poolClose                 = {#call Pool_close                 #}
poolCreate                = {#call Pool_create                #}
poolGetBusyCount          = {#call Pool_getBusyCount          #}
poolGetEncodingInfo       = {#call Pool_getEncodingInfo       #}
poolGetGetMode            = {#call Pool_getGetMode            #}
poolGetMaxLifetimeSession = {#call Pool_getMaxLifetimeSession #}
poolGetOpenCount          = {#call Pool_getOpenCount          #}
poolGetStmtCacheSize      = {#call Pool_getStmtCacheSize      #}
poolGetTimeout            = {#call Pool_getTimeout            #}
poolRelease               = {#call Pool_release               #}
poolSetGetMode            = {#call Pool_setGetMode            #}
poolSetMaxLifetimeSession = {#call Pool_setMaxLifetimeSession #}
poolSetStmtCacheSize      = {#call Pool_setStmtCacheSize      #}
poolSetTimeout            = {#call Pool_setTimeout            #}

-- Stmt
stmtAddRef             = {#call Stmt_addRef             #}
stmtBindByName         = {#call Stmt_bindByName         #}
stmtBindByPos          = {#call Stmt_bindByPos          #}
stmtBindValueByName    = {#call Stmt_bindValueByName    #}
stmtBindValueByPos     = {#call Stmt_bindValueByPos     #}
stmtClose              = {#call Stmt_close              #}
stmtDefine             = {#call Stmt_define             #}
stmtDefineValue        = {#call Stmt_defineValue        #}
stmtExecute            = {#call Stmt_execute            #}
stmtExecuteMany        = {#call Stmt_executeMany        #}
stmtFetch              = {#call Stmt_fetch              #}
stmtFetchRows          = {#call Stmt_fetchRows          #}
stmtGetBatchErrorCount = {#call Stmt_getBatchErrorCount #}
stmtGetBatchErrors     = {#call Stmt_getBatchErrors     #}
stmtGetBindCount       = {#call Stmt_getBindCount       #}
stmtGetBindNames       = {#call Stmt_getBindNames       #}
stmtGetFetchArraySize  = {#call Stmt_getFetchArraySize  #}
stmtGetImplicitResult  = {#call Stmt_getImplicitResult  #}
stmtGetInfo            = {#call Stmt_getInfo            #}
stmtGetNumQueryColumns = {#call Stmt_getNumQueryColumns #}
stmtGetQueryInfo       = {#call Stmt_getQueryInfo       #}
stmtGetQueryValue      = {#call Stmt_getQueryValue      #}
stmtGetRowCount        = {#call Stmt_getRowCount        #}
stmtGetRowCounts       = {#call Stmt_getRowCounts       #}
stmtGetSubscrQueryId   = {#call Stmt_getSubscrQueryId   #}
stmtRelease            = {#call Stmt_release            #}
stmtScroll             = {#call Stmt_scroll             #}
stmtSetFetchArraySize  = {#call Stmt_setFetchArraySize  #}

-- RowId
rowidAddRef         = {#call Rowid_addRef         #}
rowidGetStringValue = {#call Rowid_getStringValue #}
rowidRelease        = {#call Rowid_release        #}

-- Subscr
subscrAddRef      = {#call Subscr_addRef      #}
subscrClose       = {#call Subscr_close       #}
subscrPrepareStmt = {#call Subscr_prepareStmt #}
subscrRelease     = {#call Subscr_release     #}

-- Var
varAddRef                = {#call Var_addRef                #}
varCopyData              = {#call Var_copyData              #}
varGetData               = {#call Var_getData               #}
varGetNumElementsInArray = {#call Var_getNumElementsInArray #}
varGetSizeInBytes        = {#call Var_getSizeInBytes        #}
varRelease               = {#call Var_release               #}
varSetFromBytes          = {#call Var_setFromBytes          #}
varSetFromLob            = {#call Var_setFromLob            #}
varSetFromObject         = {#call Var_setFromObject         #}
varSetFromRowid          = {#call Var_setFromRowid          #}
varSetFromStmt           = {#call Var_setFromStmt           #}
varSetNumElementsInArray = {#call Var_setNumElementsInArray #}
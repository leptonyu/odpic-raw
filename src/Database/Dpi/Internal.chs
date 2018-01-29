{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}

module Database.Dpi.Internal where

import Control.Exception
import Database.Dpi.Prelude 
import qualified Data.Text as T

#include <dpi.h>

{#context prefix="dpi" #}

majorVersion :: CUInt
majorVersion = {#const DPI_MAJOR_VERSION #}

minorVersion :: CUInt
minorVersion = {#const DPI_MINOR_VERSION #}

success :: CInt
success = {#const DPI_SUCCESS #}

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

type PtrConn       = Ptr DPI_Conn
type PtrPool       = Ptr DPI_Pool
type PtrStmt       = Ptr DPI_Stmt
type PtrVar        = Ptr DPI_Var
type PtrLob        = Ptr DPI_Lob
type PtrObject     = Ptr DPI_Object
type PtrObjectAttr = Ptr DPI_ObjectAttr
type PtrObjectType = Ptr DPI_ObjectType
type PtrRowid      = Ptr DPI_Rowid
type PtrSubscr     = Ptr DPI_Subscr
type PtrDeqOptions = Ptr DPI_DeqOptions
type PtrEnqOptions = Ptr DPI_EnqOptions
type PtrMsgProps   = Ptr DPI_MsgProps
type PtrContext    = Ptr DPI_Context

--        Inner               Data
{#pointer *Bytes      as Ptr_Bytes      -> Data_Bytes      #}
{#pointer *IntervalDS as Ptr_IntervalDS -> Data_IntervalDS #}
{#pointer *IntervalYM as Ptr_IntervalYM -> Data_IntervalYM #}
{#pointer *Timestamp  as Ptr_Timestamp  -> Data_Timestamp  #}

data Data_Bytes = Data_Bytes
  { bytes    :: Text
  , encoding :: Text
  } deriving Show

instance Storable Data_Bytes where
  sizeOf    _ = {#sizeof  Bytes#}
  alignment _ = {#alignof Bytes#}
  poke      _ = noImplement
  peek      p = do
    ptr      <- {#get Bytes -> ptr      #} p
    length   <- {#get Bytes -> length   #} p
    encoding <- {#get Bytes -> encoding #} p >>= tb
    bytes    <- ts ptr length
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
  { namespaceName       :: Text
  , name                :: Text
  , value               :: Text
  } deriving Show

instance Storable Data_AppContext where
  sizeOf    _ = {#sizeof  AppContext #}
  alignment _ = {#alignof AppContext #}
  poke      _ = noImplement
  peek      p = do
    namespaceName'      <- {#get AppContext -> namespaceName       #} p
    namespaceNameLength <- {#get AppContext -> namespaceNameLength #} p
    name'               <- {#get AppContext -> name                #} p
    nameLength          <- {#get AppContext -> nameLength          #} p
    value'              <- {#get AppContext -> value               #} p
    valueLength         <- {#get AppContext -> valueLength         #} p
    namespaceName <- ts namespaceName' namespaceNameLength
    name          <- ts name'          nameLength
    value         <- ts value'         valueLength
    return Data_AppContext {..}

data Data_CommonCreateParams  = Data_CommonCreateParams
  { createMode       :: CreateMode
  , encoding         :: Text
  , nencoding        :: Text
  , edition          :: Text
  , driverName       :: Text
  } deriving Show

instance Storable Data_CommonCreateParams where
  sizeOf    _ = {#sizeof  CommonCreateParams #}
  alignment _ = {#alignof CommonCreateParams #}
  poke    p Data_CommonCreateParams{..} = do
    pe       <- fs encoding  
    pn       <- fs nencoding 
    (e,elen) <- fb edition   
    (d,dlen) <- fb driverName
    {#set CommonCreateParams -> createMode       #} p (fe createMode)
    {#set CommonCreateParams -> encoding         #} p pe
    {#set CommonCreateParams -> nencoding        #} p pn
    {#set CommonCreateParams -> edition          #} p e
    {#set CommonCreateParams -> editionLength    #} p (fromIntegral elen)
    {#set CommonCreateParams -> driverName       #} p d
    {#set CommonCreateParams -> driverNameLength #} p (fromIntegral dlen)
  peek      p = do
    createMode       <- te <$> {#get CommonCreateParams -> createMode       #} p
    encoding         <- {#get CommonCreateParams -> encoding         #} p >>= tb
    nencoding        <- {#get CommonCreateParams -> nencoding        #} p >>= tb
    edition'         <- {#get CommonCreateParams -> edition          #} p
    editionLength    <- {#get CommonCreateParams -> editionLength    #} p
    driverName'      <- {#get CommonCreateParams -> driverName       #} p
    driverNameLength <- {#get CommonCreateParams -> driverNameLength #} p
    edition    <- ts edition'    editionLength
    driverName <- ts driverName' driverNameLength
    return Data_CommonCreateParams {..}

data Data_ConnCreateParams  = Data_ConnCreateParams
  { authMode                   :: AuthMode
  , connectionClass            :: Text
  , purity                     :: Purity
  , newPassword                :: Text
  , appContext                 :: PtrAppContext
  , numAppContext              :: CUInt
  , externalAuth               :: CInt
  , externalHandle             :: Ptr ()
  , pool                       :: PtrPool
  , tag                        :: Text
  , matchAnyTag                :: CInt
  , outTag                     :: Text
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
    connectionClass'           <- {#get ConnCreateParams -> connectionClass            #} p
    connectionClassLength      <- {#get ConnCreateParams -> connectionClassLength      #} p
    purity                     <- te <$> {#get ConnCreateParams -> purity                     #} p
    newPassword'               <- {#get ConnCreateParams -> newPassword                #} p
    newPasswordLength          <- {#get ConnCreateParams -> newPasswordLength          #} p
    appContext                 <- {#get ConnCreateParams -> appContext                 #} p
    numAppContext              <- {#get ConnCreateParams -> numAppContext              #} p
    externalAuth               <- {#get ConnCreateParams -> externalAuth               #} p
    externalHandle             <- {#get ConnCreateParams -> externalHandle             #} p
    pool                       <- {#get ConnCreateParams -> pool                       #} p
    tag'                       <- {#get ConnCreateParams -> tag                        #} p
    tagLength                  <- {#get ConnCreateParams -> tagLength                  #} p
    matchAnyTag                <- {#get ConnCreateParams -> matchAnyTag                #} p
    outTag'                    <- {#get ConnCreateParams -> outTag                     #} p
    outTagLength               <- {#get ConnCreateParams -> outTagLength               #} p
    outTagFound                <- {#get ConnCreateParams -> outTagFound                #} p
    shardingKeyColumns         <- {#get ConnCreateParams -> shardingKeyColumns         #} p
    numShardingKeyColumns      <- {#get ConnCreateParams -> numShardingKeyColumns      #} p
    superShardingKeyColumns    <- {#get ConnCreateParams -> superShardingKeyColumns    #} p
    numSuperShardingKeyColumns <- {#get ConnCreateParams -> numSuperShardingKeyColumns #} p
    connectionClass <- ts connectionClass' connectionClassLength
    newPassword     <- ts newPassword'     newPasswordLength
    tag             <- ts tag'             tagLength
    outTag          <- ts outTag'          outTagLength
    return Data_ConnCreateParams {..}

data DataValue
  = DataNull       NativeTypeNum
  | DataInt64      CLLong
  | DataUint64     CULLong
  | DataFloat      CFloat
  | DataDouble     CDouble
  | DataBytes      Data_Bytes
  | DataTimestamp  Data_Timestamp
  | DataIntervalDs Data_IntervalDS
  | DataIntervalYm Data_IntervalYM
  | DataLob        PtrLob
  | DataObject     PtrObject
  | DataStmt       PtrStmt
  | DataBoolean    Bool
  | DataRowid      PtrRowid
  deriving Show

newData :: DataValue -> IO (NativeTypeNum, PtrData)
newData d = do
  pd <- malloc
  let tp = go d
  poke pd (Data $ \_ -> return d)
  return (tp, pd)
  where
    go (DataNull       t) = t
    go (DataInt64      _) = NativeTypeInt64
    go (DataUint64     _) = NativeTypeUint64
    go (DataFloat      _) = NativeTypeFloat
    go (DataDouble     _) = NativeTypeDouble
    go (DataBytes      _) = NativeTypeBytes
    go (DataTimestamp  _) = NativeTypeTimestamp
    go (DataIntervalDs _) = NativeTypeIntervalDs
    go (DataIntervalYm _) = NativeTypeIntervalYm
    go (DataLob        _) = NativeTypeLob
    go (DataObject     _) = NativeTypeObject
    go (DataStmt       _) = NativeTypeStmt
    go (DataBoolean    _) = NativeTypeBoolean
    go (DataRowid      _) = NativeTypeRowid

newtype Data = Data (NativeTypeNum -> IO DataValue)

instance Storable Data where
  sizeOf    _ = {#sizeof  Data #}
  alignment _ = {#alignof Data #}
  poke      p (Data f) = do
    d <- f NativeTypeBoolean
    go p d
    where
      go p (DataNull       _) = {#set Data -> isNull             #} p 1
      go p (DataInt64      v) = {#set Data -> value.asInt64      #} p v
      go p (DataUint64     v) = {#set Data -> value.asUint64     #} p v
      go p (DataFloat      v) = {#set Data -> value.asFloat      #} p v
      go p (DataDouble     v) = {#set Data -> value.asDouble     #} p v
      go p (DataBytes      (Data_Bytes {..})) = do
        (b,bl) <- fb bytes
        e      <- fs encoding
        {#set Data -> value.asBytes.ptr      #} p b
        {#set Data -> value.asBytes.length   #} p (fromIntegral bl)
        {#set Data -> value.asBytes.encoding #} p e
      go p (DataTimestamp  (Data_Timestamp {..})) = do
        {#set Data -> value.asTimestamp.year           #} p year          
        {#set Data -> value.asTimestamp.month          #} p month         
        {#set Data -> value.asTimestamp.day            #} p day           
        {#set Data -> value.asTimestamp.hour           #} p hour          
        {#set Data -> value.asTimestamp.minute         #} p minute        
        {#set Data -> value.asTimestamp.second         #} p second        
        {#set Data -> value.asTimestamp.fsecond        #} p fsecond       
        {#set Data -> value.asTimestamp.tzHourOffset   #} p tzHourOffset  
        {#set Data -> value.asTimestamp.tzMinuteOffset #} p tzMinuteOffset
      go p (DataIntervalDs (Data_IntervalDS {..})) = do
        {#set Data -> value.asIntervalDS.days     #} p days    
        {#set Data -> value.asIntervalDS.hours    #} p hours   
        {#set Data -> value.asIntervalDS.minutes  #} p minutes 
        {#set Data -> value.asIntervalDS.seconds  #} p seconds 
        {#set Data -> value.asIntervalDS.fseconds #} p fseconds
      go p (DataIntervalYm (Data_IntervalYM {..})) = do
        {#set Data -> value.asIntervalYM.years    #} p  years 
        {#set Data -> value.asIntervalYM.months   #} p  months
      go p (DataLob        v) = {#set Data -> value.asLOB        #} p v
      go p (DataObject     v) = {#set Data -> value.asObject#}   p  v
      go p (DataStmt       v) = {#set Data -> value.asStmt       #} p v
      go p (DataRowid      v) = {#set Data -> value.asRowid      #} p v
      go p (DataBoolean    v) = {#set Data -> value.asBoolean    #} p (fromBool v)
  peek      p = return $ Data $ go' p
    where
      go' p t = do
        isNull <- toBool <$> {#get Data -> isNull #} p
        if isNull 
          then return $ DataNull t
          else go p t
      go p NativeTypeInt64      = DataInt64      <$> {#get Data -> value.asInt64      #} p
      go p NativeTypeUint64     = DataUint64     <$> {#get Data -> value.asUint64     #} p
      go p NativeTypeFloat      = DataFloat      <$> {#get Data -> value.asFloat      #} p
      go p NativeTypeDouble     = DataDouble     <$> {#get Data -> value.asDouble     #} p
      go p NativeTypeBytes      = DataBytes      <$> do
        ptr      <- {#get Data -> value.asBytes.ptr      #} p
        length   <- {#get Data -> value.asBytes.length   #} p
        encoding <- {#get Data -> value.asBytes.encoding #} p >>= tb
        bytes    <- ts ptr length
        return Data_Bytes {..}
      go p NativeTypeTimestamp  = DataTimestamp  <$> do
        year           <- {#get Data -> value.asTimestamp.year           #} p
        month          <- {#get Data -> value.asTimestamp.month          #} p
        day            <- {#get Data -> value.asTimestamp.day            #} p
        hour           <- {#get Data -> value.asTimestamp.hour           #} p
        minute         <- {#get Data -> value.asTimestamp.minute         #} p
        second         <- {#get Data -> value.asTimestamp.second         #} p
        fsecond        <- {#get Data -> value.asTimestamp.fsecond        #} p
        tzHourOffset   <- {#get Data -> value.asTimestamp.tzHourOffset   #} p
        tzMinuteOffset <- {#get Data -> value.asTimestamp.tzMinuteOffset #} p
        return Data_Timestamp {..}
      go p NativeTypeIntervalDs = DataIntervalDs <$> do
        days     <- {#get Data -> value.asIntervalDS.days     #} p
        hours    <- {#get Data -> value.asIntervalDS.hours    #} p
        minutes  <- {#get Data -> value.asIntervalDS.minutes  #} p
        seconds  <- {#get Data -> value.asIntervalDS.seconds  #} p
        fseconds <- {#get Data -> value.asIntervalDS.fseconds #} p
        return Data_IntervalDS {..}
      go p NativeTypeIntervalYm = DataIntervalYm <$> do
        years    <- {#get Data -> value.asIntervalYM.years    #} p
        months   <- {#get Data -> value.asIntervalYM.months   #} p
        return Data_IntervalYM {..}
      go p NativeTypeLob        = DataLob        <$> {#get Data -> value.asLOB        #} p
      go p NativeTypeObject     = DataObject     <$> {#get Data -> value.asObject     #} p
      go p NativeTypeStmt       = DataStmt       <$> {#get Data -> value.asStmt       #} p
      go p NativeTypeRowid      = DataRowid      <$> {#get Data -> value.asRowid      #} p
      go p NativeTypeBoolean    = (DataBoolean .toBool)<$> {#get Data -> value.asBoolean    #} p

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
  , objectType           :: PtrObjectType
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
  { encoding              :: Text
  , maxBytesPerCharacter  :: CInt
  , nencoding             :: Text
  , nmaxBytesPerCharacter :: CInt
  } deriving Show

instance Storable Data_EncodingInfo where
  sizeOf    _ = {#sizeof  EncodingInfo #}
  alignment _ = {#alignof EncodingInfo #}
  poke      _ = noImplement
  peek      p = do
    encoding              <- {#get EncodingInfo -> encoding              #} p >>= tb
    maxBytesPerCharacter  <- {#get EncodingInfo -> maxBytesPerCharacter  #} p
    nencoding             <- {#get EncodingInfo -> nencoding             #} p >>= tb
    nmaxBytesPerCharacter <- {#get EncodingInfo -> nmaxBytesPerCharacter #} p
    return Data_EncodingInfo {..}

data Data_ErrorInfo  = Data_ErrorInfo
  { code          :: CInt
  , offset        :: CUShort
  , message       :: Text
  , encoding      :: Text
  , fnName        :: Text
  , action        :: Text
  , sqlState      :: Text
  , isRecoverable :: Bool
  } deriving Show

instance Storable Data_ErrorInfo where
  sizeOf    _ = {#sizeof  ErrorInfo #}
  alignment _ = {#alignof ErrorInfo #}
  poke      _ = noImplement
  peek      p = do
    code          <- {#get ErrorInfo -> code          #} p
    offset        <- {#get ErrorInfo -> offset        #} p
    message'      <- {#get ErrorInfo -> message       #} p
    messageLength <- {#get ErrorInfo -> messageLength #} p
    encoding      <- {#get ErrorInfo -> encoding      #} p >>= tb
    fnName        <- {#get ErrorInfo -> fnName        #} p >>= tb
    action        <- {#get ErrorInfo -> action        #} p >>= tb
    sqlState      <- {#get ErrorInfo -> sqlState      #} p >>= tb
    isRecoverable <- toBool <$> {#get ErrorInfo -> isRecoverable #} p
    message       <- ts message' messageLength
    return Data_ErrorInfo {..}

data Data_ObjectAttrInfo  = Data_ObjectAttrInfo
  { name       :: Text
  , typeInfo   :: Data_DataTypeInfo
  } deriving Show

instance Storable Data_ObjectAttrInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke      _ = noImplement
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
    name       <- ts name' nameLength
    return Data_ObjectAttrInfo {..}

data Data_ObjectTypeInfo  = Data_ObjectTypeInfo
  { schema          :: Text
  , name            :: Text
  , isCollection    :: Bool
  , elementTypeInfo :: Data_DataTypeInfo
  , numAttributes   :: CUShort
  } deriving Show

instance Storable Data_ObjectTypeInfo where
  sizeOf    _ = {#sizeof  ObjectAttrInfo #}
  alignment _ = {#alignof ObjectAttrInfo #}
  poke      _ = noImplement
  peek      p = do
    schema'         <- {#get ObjectTypeInfo -> schema          #} p
    schemaLength    <- {#get ObjectTypeInfo -> schemaLength    #} p
    name'           <- {#get ObjectTypeInfo -> name            #} p
    nameLength      <- {#get ObjectTypeInfo -> nameLength      #} p
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
    schema          <- ts schema' schemaLength
    name            <- ts name'   nameLength
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
  , outPoolName       :: Text
  } deriving Show

instance Storable Data_PoolCreateParams where
  sizeOf    _ = {#sizeof  PoolCreateParams #}
  alignment _ = {#alignof PoolCreateParams #}
  poke      p Data_PoolCreateParams{..} = do
    (e,elen) <- fb outPoolName   
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
    minSessions       <- {#get PoolCreateParams -> minSessions       #} p
    maxSessions       <- {#get PoolCreateParams -> maxSessions       #} p
    sessionIncrement  <- {#get PoolCreateParams -> sessionIncrement  #} p
    pingInterval      <- {#get PoolCreateParams -> pingInterval      #} p
    pingTimeout       <- {#get PoolCreateParams -> pingTimeout       #} p
    homogeneous       <- {#get PoolCreateParams -> homogeneous       #} p
    externalAuth      <- {#get PoolCreateParams -> externalAuth      #} p
    getMode           <- te <$> {#get PoolCreateParams -> getMode           #} p
    outPoolName'      <- {#get PoolCreateParams -> outPoolName       #} p
    outPoolNameLength <- {#get PoolCreateParams -> outPoolNameLength #} p
    outPoolName       <- ts outPoolName' outPoolNameLength
    return Data_PoolCreateParams {..}

data Data_QueryInfo = Data_QueryInfo
  { name       :: Text
  , typeInfo   :: Data_DataTypeInfo
  , nullOk     :: Bool
  } deriving Show

instance Storable Data_QueryInfo where
  sizeOf    _ = {#sizeof  QueryInfo #}
  alignment _ = {#alignof QueryInfo #}
  poke      _ = noImplement
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
    name       <- ts name' nameLength
    return Data_QueryInfo {..}

data Data_ShardingKeyColumn  = Data_ShardingKeyColumn
  { oracleTypeNum :: OracleTypeNum
  , nativeTypeNum :: NativeTypeNum
  , value         :: DataValue
  } deriving Show

instance Storable Data_ShardingKeyColumn where
  sizeOf    _ = {#sizeof  ShardingKeyColumn #}
  alignment _ = {#alignof ShardingKeyColumn #}
  poke      _ = noImplement
  peek      p = do
    oracleTypeNum <- te      <$> {#get ShardingKeyColumn -> oracleTypeNum #} p
    nativeTypeNum <- te      <$> {#get ShardingKeyColumn -> nativeTypeNum #} p
    value         <- go p nativeTypeNum
    return Data_ShardingKeyColumn {..}
    where
      go p NativeTypeInt64      = DataInt64      <$> {#get ShardingKeyColumn -> value.asInt64      #} p
      go p NativeTypeUint64     = DataUint64     <$> {#get ShardingKeyColumn -> value.asUint64     #} p
      go p NativeTypeFloat      = DataFloat      <$> {#get ShardingKeyColumn -> value.asFloat      #} p
      go p NativeTypeDouble     = DataDouble     <$> {#get ShardingKeyColumn -> value.asDouble     #} p
      go p NativeTypeBytes      = DataBytes      <$> do
        ptr      <- {#get ShardingKeyColumn -> value.asBytes.ptr      #} p
        length   <- {#get ShardingKeyColumn -> value.asBytes.length   #} p
        encoding <- {#get ShardingKeyColumn -> value.asBytes.encoding #} p >>= tb
        bytes    <- ts ptr length
        return Data_Bytes {..}
      go p NativeTypeTimestamp  = DataTimestamp  <$> do
        year           <- {#get ShardingKeyColumn -> value.asTimestamp.year           #} p
        month          <- {#get ShardingKeyColumn -> value.asTimestamp.month          #} p
        day            <- {#get ShardingKeyColumn -> value.asTimestamp.day            #} p
        hour           <- {#get ShardingKeyColumn -> value.asTimestamp.hour           #} p
        minute         <- {#get ShardingKeyColumn -> value.asTimestamp.minute         #} p
        second         <- {#get ShardingKeyColumn -> value.asTimestamp.second         #} p
        fsecond        <- {#get ShardingKeyColumn -> value.asTimestamp.fsecond        #} p
        tzHourOffset   <- {#get ShardingKeyColumn -> value.asTimestamp.tzHourOffset   #} p
        tzMinuteOffset <- {#get ShardingKeyColumn -> value.asTimestamp.tzMinuteOffset #} p
        return Data_Timestamp {..}
      go p NativeTypeIntervalDs = DataIntervalDs <$> do
        days     <- {#get ShardingKeyColumn -> value.asIntervalDS.days     #} p
        hours    <- {#get ShardingKeyColumn -> value.asIntervalDS.hours    #} p
        minutes  <- {#get ShardingKeyColumn -> value.asIntervalDS.minutes  #} p
        seconds  <- {#get ShardingKeyColumn -> value.asIntervalDS.seconds  #} p
        fseconds <- {#get ShardingKeyColumn -> value.asIntervalDS.fseconds #} p
        return Data_IntervalDS {..}
      go p NativeTypeIntervalYm = DataIntervalYm <$> do
        years    <- {#get ShardingKeyColumn -> value.asIntervalYM.years    #} p
        months   <- {#get ShardingKeyColumn -> value.asIntervalYM.months   #} p
        return Data_IntervalYM {..}
      go p NativeTypeLob        = DataLob        <$> {#get ShardingKeyColumn -> value.asLOB        #} p
      go p NativeTypeObject     = DataObject     <$> {#get ShardingKeyColumn -> value.asObject     #} p
      go p NativeTypeStmt       = DataStmt       <$> {#get ShardingKeyColumn -> value.asStmt       #} p
      go p NativeTypeRowid      = DataRowid      <$> {#get ShardingKeyColumn -> value.asRowid      #} p
      go p NativeTypeBoolean    = (DataBoolean .toBool)<$> {#get ShardingKeyColumn -> value.asBoolean    #} p

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
  , name                :: Text
  , callback            :: FunPtr (Ptr () -> PtrSubscrMessage -> IO ())
  , callbackContext     :: Ptr ()
  , recipientName       :: Text
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
    name'               <- {#get SubscrCreateParams ->    name                #} p
    nameLength          <- {#get SubscrCreateParams ->    nameLength          #} p
    callback            <- {#get SubscrCreateParams ->    callback            #} p
    callbackContext     <- {#get SubscrCreateParams ->    callbackContext     #} p
    recipientName'      <- {#get SubscrCreateParams ->    recipientName       #} p
    recipientNameLength <- {#get SubscrCreateParams ->    recipientNameLength #} p
    name                <- ts name'          nameLength
    recipientName       <- ts recipientName' recipientNameLength
    return Data_SubscrCreateParams {..}

data Data_SubscrMessage = Data_SubscrMessage
  { eventType    :: EventType
  , dbName       :: Text
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
    dbName'      <- {#get SubscrMessage -> dbName       #} p
    dbNameLength <- {#get SubscrMessage -> dbNameLength #} p
    tables       <- {#get SubscrMessage -> tables       #} p
    numTables    <- {#get SubscrMessage -> numTables    #} p
    queries      <- {#get SubscrMessage -> queries      #} p
    numQueries   <- {#get SubscrMessage -> numQueries   #} p
    errorInfo    <- {#get SubscrMessage -> errorInfo    #} p
    txId         <- {#get SubscrMessage -> txId         #} p
    txIdLength   <- {#get SubscrMessage -> txIdLength   #} p
    dbName       <- ts dbName' dbNameLength
    return Data_SubscrMessage {..}

data Data_SubscrMessageQuery = Data_SubscrMessageQuery
  { mid       :: CULLong
  , operation :: OpCode
  , tables    :: PtrSubscrMessageTable
  , numTables :: CUInt
  } deriving Show

instance Storable Data_SubscrMessageQuery where
  sizeOf    _ = {#sizeof  SubscrMessageQuery #}
  alignment _ = {#alignof SubscrMessageQuery #}
  poke      _ = noImplement
  peek      p = do
    mid       <- {#get SubscrMessageQuery -> id        #} p
    operation <- te <$> {#get SubscrMessageQuery -> operation #} p
    tables    <- {#get SubscrMessageQuery -> tables    #} p
    numTables <- {#get SubscrMessageQuery -> numTables #} p
    return Data_SubscrMessageQuery {..}

data Data_SubscrMessageRow = Data_SubscrMessageRow
  { operation   :: OpCode
  , rowid       :: Text
  } deriving Show

instance Storable Data_SubscrMessageRow where
  sizeOf    _ = {#sizeof  SubscrMessageRow #}
  alignment _ = {#alignof SubscrMessageRow #}
  poke      _ = noImplement
  peek      p = do
    operation   <- te <$> {#get SubscrMessageRow -> operation  #} p
    rowid'      <- {#get SubscrMessageRow -> rowid       #} p
    rowidLength <- {#get SubscrMessageRow -> rowidLength #} p
    rowid       <- ts rowid' rowidLength
    return Data_SubscrMessageRow {..}

data Data_SubscrMessageTable = Data_SubscrMessageTable
  { operation  :: OpCode
  , name       :: Text
  , rows       :: PtrSubscrMessageRow
  , numRows    :: CUInt
  } deriving Show

instance Storable Data_SubscrMessageTable where
  sizeOf    _ = {#sizeof  SubscrMessageTable #}
  alignment _ = {#alignof SubscrMessageTable #}
  poke      _ = noImplement
  peek      p = do
    operation  <- te <$> {#get SubscrMessageTable -> operation  #} p
    name'      <- {#get SubscrMessageTable -> name       #} p
    nameLength <- {#get SubscrMessageTable -> nameLength #} p
    rows       <- {#get SubscrMessageTable -> rows       #} p
    numRows    <- {#get SubscrMessageTable -> numRows    #} p
    name       <- ts name' nameLength
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
libContextCreate                 = {#call Context_create                 #}
libContextDestroy                = {#call Context_destroy                #}
libContextGetClientVersion       = {#call Context_getClientVersion       #}
libContextInitCommonCreateParams = {#call Context_initCommonCreateParams #}
libContextInitConnCreateParams   = {#call Context_initConnCreateParams   #}
libContextInitPoolCreateParams   = {#call Context_initPoolCreateParams   #}
libContextInitSubscrCreateParams = {#call Context_initSubscrCreateParams #}
libContextGetError               = {#call Context_getError               #}

-- Conn

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
libConnNewSubscription     = {#call Conn_newSubscription     #}
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

-- Data 
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
libEnqOptionsAddRef            = {#call EnqOptions_addRef            #}
libEnqOptionsGetTransformation = {#call EnqOptions_getTransformation #}
libEnqOptionsGetVisibility     = {#call EnqOptions_getVisibility     #}
libEnqOptionsRelease           = {#call EnqOptions_release           #}
libEnqOptionsSetDeliveryMode   = {#call EnqOptions_setDeliveryMode   #}
libEnqOptionsSetTransformation = {#call EnqOptions_setTransformation #}
libEnqOptionsSetVisibility     = {#call EnqOptions_setVisibility     #}

-- Lob
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
libObjectAttrAddRef  = {#call ObjectAttr_addRef  #}
libObjectAttrGetInfo = {#call ObjectAttr_getInfo #}
libObjectAttrRelease = {#call ObjectAttr_release #}

-- ObjectType
libObjectTypeAddRef        = {#call ObjectType_addRef        #}
libObjectTypeCreateObject  = {#call ObjectType_createObject  #}
libObjectTypeGetAttributes = {#call ObjectType_getAttributes #}
libObjectTypeGetInfo       = {#call ObjectType_getInfo       #}
libObjectTypeRelease       = {#call ObjectType_release       #}

-- Pool
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

-- Stmt
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
libRowidAddRef         = {#call Rowid_addRef         #}
libRowidGetStringValue = {#call Rowid_getStringValue #}
libRowidRelease        = {#call Rowid_release        #}

-- Subscr
libSubscrAddRef      = {#call Subscr_addRef      #}
libSubscrClose       = {#call Subscr_close       #}
libSubscrPrepareStmt = {#call Subscr_prepareStmt #}
libSubscrRelease     = {#call Subscr_release     #}

-- Var
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
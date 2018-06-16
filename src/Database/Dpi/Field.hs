{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Dpi.Field(
    DataField(..)
  , FromDataField(..)
  , ToDataField(..)
  , isNullable
  ) where

import           Database.Dpi.Internal
import           Database.Dpi.Util

import           Control.Exception      (throw)
import           Data.Decimal
import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as B
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word
import           Foreign.C.Types

-- | Database Raw Data with Type Info
data DataField = DataField
  { info  :: !Data_QueryInfo -- ^ Type Info
  , value :: !DataValue      -- ^ Raw Value
  } deriving (Show)

-- | Some Type can convert from 'DataField'
class FromDataField a where
  fromDataField  :: DataField -> IO (Maybe a)

-- | Check if data field is nullable
{-# INLINE isNullable #-}
isNullable    :: DataField -> Bool
isNullable DataField{..} = let Data_QueryInfo{..} = info in nullOk

instance FromDataField Text where
  fromDataField = fmap (fmap L.toStrict) . fromDataField

instance FromDataField L.Text where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataChar          v) = return . Just $ toLazyText v
      go _ _ (DataLongRaw       v) = return . Just $ toLazyText v
      go _ _ (DataLongVarchar   v) = return . Just $ toLazyText v
      go _ _ (DataNVarchar      v) = return . Just $ toLazyText v
      go _ _ (DataRaw           v) = return . Just $ toLazyText v
      go _ _ (DataVarchar       v) = return . Just $ toLazyText v
      go _ _ (DataNChar         v) = return . Just $ toLazyText v
      go n _ _                     = singleError n

instance FromDataField Integer where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataInt           v) = return . Just $ toInteger v
      go _ _ (DataUint          v) = return . Just $ toInteger v
      go _ _ (DataNumInt        v) = return . Just $ toInteger v
      go _ _ (DataNumUint       v) = return . Just $ toInteger v
      go _ _ (DataFloat         v) = return . Just $ round     v
      go _ _ (DataNumDouble     v) = return . Just $ round     v
      go _ _ (DataDouble        v) = return . Just $ round     v
      -- go _ _ (DataNumBytes      v) = !Data_Bytes
      go n _ _                     = singleError n

instance FromDataField Int where
  fromDataField = fmap (fmap fromInteger) . fromDataField

instance FromDataField Int64 where
  fromDataField = fmap (fmap fromInteger) . fromDataField

instance FromDataField Word where
  fromDataField = fmap (fmap fromInteger) . fromDataField

instance FromDataField Word64 where
  fromDataField = fmap (fmap fromInteger) . fromDataField

instance FromDataField Double where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataInt           v) = return . Just $ realToFrac v
      go _ _ (DataUint          v) = return . Just $ realToFrac v
      go _ _ (DataNumInt        v) = return . Just $ realToFrac v
      go _ _ (DataNumUint       v) = return . Just $ realToFrac v
      go _ _ (DataFloat         v) = return . Just $ realToFrac v
      go _ _ (DataNumDouble     v) = return . Just $ realToFrac v
      go _ _ (DataDouble        v) = return . Just $ realToFrac v
      -- go _ _ (DataNumBytes      v) = !Data_Bytes
      go n _ _                     = singleError n

instance FromDataField Float where
  fromDataField = fmap (fmap (realToFrac :: Double -> Float)) . fromDataField

instance FromDataField Decimal where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataInt           v) = return . Just $ realToFrac v
      go _ _ (DataUint          v) = return . Just $ realToFrac v
      go _ _ (DataNumInt        v) = return . Just $ realToFrac v
      go _ _ (DataNumUint       v) = return . Just $ realToFrac v
      go _ _ (DataFloat         v) = return . Just $ realToFrac v
      go _ _ (DataNumDouble     v) = return . Just $ realToFrac v
      go _ _ (DataDouble        v) = return . Just $ realToFrac v
      -- go _ _ (DataNumBytes      v) = !Data_Bytes
      go n _ _                     = singleError n

instance FromDataField Bool where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataBoolean       v) = return $ Just v
      go n _ _                     = singleError n

instance FromDataField UTCTime where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataDate          v) = return . Just $ toUTCTime  v
      go _ _ (DataTimestamp     v) = return . Just $ toUTCTime  v
      go _ _ (DataTimestampLtz  v) = return . Just $ toUTCTimeL v
      go _ _ (DataTimestampTz   v) = return . Just $ toUTCTime  v
      go _ _ (DataTimestampD    v) = return . Just $ toUTCTimeD v
      go _ _ (DataTimestampLtzD v) = return . Just $ toUTCTimeD v
      go _ _ (DataTimestampTzD  v) = return . Just $ toUTCTimeD v
      go n _ _                     = singleError n
      -- go _ _ (DataIntervalDs    v) = !Data_IntervalDS
      -- go _ _ (DataIntervalYm    v) = !Data_IntervalYM

instance FromDataField ZonedTime where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataDate          v) = return . Just $ toZonedTime False v
      go _ _ (DataTimestamp     v) = return . Just $ toZonedTime False v
      go _ _ (DataTimestampLtz  v) = return . Just $ toZonedTime True  v
      go _ _ (DataTimestampTz   v) = return . Just $ toZonedTime False v
      go _ _ (DataTimestampD    v) = return . Just $ utcToZonedTime utc $ toUTCTimeD v
      go _ _ (DataTimestampLtzD v) = return . Just $ utcToZonedTime utc $ toUTCTimeD v
      go _ _ (DataTimestampTzD  v) = return . Just $ utcToZonedTime utc $ toUTCTimeD v
      go n _ _                     = singleError n
      -- go _ _ (DataIntervalDs    v) = !Data_IntervalDS
      -- go _ _ (DataIntervalYm    v) = !Data_IntervalYM

instance FromDataField LocalTime where
  fromDataField = fmap (fmap go) . fromDataField
    where
      go ZonedTime{..} = zonedTimeToLocalTime

instance FromDataField DiffTime where
  fromDataField DataField{..} = let Data_QueryInfo{..} = info in go name typeInfo value
    where
      go _ _ (DataNull          _) = return Nothing
      go _ _ (DataIntervalDs    v) = return . Just $ toDiffTime  v
      go _ _ (DataIntervalYm    v) = return . Just $ toDiffTime' v
      go n _ _                     = singleError n

-- | Some type can convert to 'DataValue'
class ToDataField a where
  toDataField :: a -> NativeTypeNum -> OracleTypeNum -> IO DataValue

instance ToDataField Text where
  toDataField v = toDataField (B.toLazyText $ B.fromText v)

instance ToDataField L.Text where
  toDataField v NativeTypeBytes OracleTypeChar        = return $ DataChar        $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeLongRaw     = return $ DataLongRaw     $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeLongVarchar = return $ DataLongVarchar $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeNchar       = return $ DataNChar       $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeNumber      = return $ DataNumBytes    $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeNvarchar    = return $ DataNVarchar    $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeRaw         = return $ DataRaw         $ fromLazyText v
  toDataField v NativeTypeBytes OracleTypeVarchar     = return $ DataVarchar     $ fromLazyText v
  toDataField _ _               _                     = singleError     "Text"

instance ToDataField Bool where
  toDataField v NativeTypeBoolean OracleTypeBoolean = return $ DataBoolean v
  toDataField _ _                 _                 = singleError "Bool"

instance ToDataField Integer where
  toDataField v NativeTypeDouble OracleTypeNativeDouble = return $ DataDouble    $ realToFrac v
  toDataField v NativeTypeDouble OracleTypeNumber       = return $ DataNumDouble $ realToFrac v
  toDataField v NativeTypeFloat  OracleTypeNativeFloat  = return $ DataFloat     $ realToFrac v
  toDataField v NativeTypeInt64  OracleTypeNativeInt    = return $ DataInt       $ fromIntegral v
  toDataField v NativeTypeInt64  OracleTypeNumber       = return $ DataNumInt    $ fromIntegral v
  toDataField v NativeTypeUint64 OracleTypeNativeUint   = return $ DataUint      $ fromIntegral v
  toDataField v NativeTypeUint64 OracleTypeNumber       = return $ DataNumUint   $ fromIntegral v
  toDataField _ _                 _                 = singleError "Integer"

instance ToDataField Int where
  toDataField v = toDataField (toInteger v)

instance ToDataField Int64 where
  toDataField v = toDataField (toInteger v)

instance ToDataField Word where
  toDataField v = toDataField (toInteger v)

instance ToDataField Word64 where
  toDataField v = toDataField (toInteger v)

instance ToDataField Decimal where
  toDataField v NativeTypeDouble OracleTypeNativeDouble = return $ DataDouble    $ realToFrac v
  toDataField v NativeTypeDouble OracleTypeNumber       = return $ DataNumDouble $ realToFrac v
  toDataField v NativeTypeFloat  OracleTypeNativeFloat  = return $ DataFloat     $ realToFrac v
  toDataField v NativeTypeInt64  OracleTypeNativeInt    = return $ DataInt       $ round v
  toDataField v NativeTypeInt64  OracleTypeNumber       = return $ DataNumInt    $ round v
  toDataField v NativeTypeUint64 OracleTypeNativeUint   = return $ DataUint      $ round v
  toDataField v NativeTypeUint64 OracleTypeNumber       = return $ DataNumUint   $ round v
  toDataField _ _                 _                 = singleError "Decimal"

instance ToDataField Double where
  toDataField v NativeTypeDouble OracleTypeNativeDouble = return $ DataDouble    $ realToFrac v
  toDataField v NativeTypeDouble OracleTypeNumber       = return $ DataNumDouble $ realToFrac v
  toDataField v NativeTypeFloat  OracleTypeNativeFloat  = return $ DataFloat     $ realToFrac v
  toDataField v NativeTypeInt64  OracleTypeNativeInt    = return $ DataInt       $ round v
  toDataField v NativeTypeInt64  OracleTypeNumber       = return $ DataNumInt    $ round v
  toDataField v NativeTypeUint64 OracleTypeNativeUint   = return $ DataUint      $ round v
  toDataField v NativeTypeUint64 OracleTypeNumber       = return $ DataNumUint   $ round v
  toDataField _ _                 _                 = singleError "Double"

instance ToDataField Float where
  toDataField v = toDataField (realToFrac v :: Double)

instance ToDataField UTCTime where
  toDataField v NativeTypeTimestamp OracleTypeDate         = return $ DataDate          $ fromUTCTime  v
  toDataField v NativeTypeDouble    OracleTypeTimestamp    = return $ DataTimestampD    $ fromUTCTimeD v
  toDataField v NativeTypeDouble    OracleTypeTimestampLtz = return $ DataTimestampLtzD $ fromUTCTimeD v
  toDataField v NativeTypeDouble    OracleTypeTimestampTz  = return $ DataTimestampTzD  $ fromUTCTimeD v
  toDataField v NativeTypeTimestamp OracleTypeTimestamp    = return $ DataTimestamp     $ fromUTCTime  v
  toDataField v NativeTypeTimestamp OracleTypeTimestampLtz = return $ DataTimestampLtz  $ fromUTCTime  v
  toDataField v NativeTypeTimestamp OracleTypeTimestampTz  = return $ DataTimestampTz   $ fromUTCTime  v
  toDataField _ _                 _                 = singleError "UTCTime"

instance ToDataField ZonedTime where
  toDataField v NativeTypeTimestamp OracleTypeDate         = return $ DataDate          $ fromZonedTime v
  -- toDataField v NativeTypeDouble    OracleTypeTimestamp    = return $ DataTimestampD    $ fromUTCTimeD  v
  -- toDataField v NativeTypeDouble    OracleTypeTimestampLtz = return $ DataTimestampLtzD $ fromUTCTimeD  v
  -- toDataField v NativeTypeDouble    OracleTypeTimestampTz  = return $ DataTimestampTzD  $ fromUTCTimeD  v
  toDataField v NativeTypeTimestamp OracleTypeTimestamp    = return $ DataTimestamp     $ fromZonedTime v
  toDataField v NativeTypeTimestamp OracleTypeTimestampLtz = return $ DataTimestampLtz  $ fromZonedTime v
  toDataField v NativeTypeTimestamp OracleTypeTimestampTz  = return $ DataTimestampTz   $ fromZonedTime v
  toDataField _ _                 _                 = singleError "ZonedTime"

instance ToDataField DiffTime where
  toDataField v NativeTypeIntervalDs OracleTypeIntervalDs = return $ DataIntervalDs $ fromDiffTime  v
  toDataField v NativeTypeIntervalYm OracleTypeIntervalYm = return $ DataIntervalYm $ fromDiffTime' v
  toDataField _ _                 _                 = singleError "DiffTime"


{-# INLINE fromLazyText #-}
fromLazyText :: L.Text -> Data_Bytes
fromLazyText bytes = let encoding = "utf-8" in Data_Bytes{..}

{-# INLINE toLazyText #-}
toLazyText :: Data_Bytes -> L.Text
toLazyText Data_Bytes{..} = bytes

{-# INLINE fromDiffTime #-}
fromDiffTime :: DiffTime -> Data_IntervalDS
fromDiffTime dt =
  let dts           = diffTimeToPicoseconds dt
      (r1,fseconds) = dts `divMod` (10^12)
      (r2,seconds)  = r1  `divMod` 60
      (r3,minutes)  = r2  `divMod` 60
      (days,hours)  = r3  `divMod` 24
  in Data_IntervalDS (fromInteger days) (fromInteger hours) (fromInteger minutes) (fromInteger seconds) (fromInteger fseconds)

{-# INLINE fromDiffTime' #-}
fromDiffTime' :: DiffTime -> Data_IntervalYM
fromDiffTime' dt =
  let dts   = diffTimeToPicoseconds dt `div` (30 * 86400 * 10^12)
      (y,m) = dts `divMod` 12
  in Data_IntervalYM (fromInteger y) (fromInteger m)

{-# INLINE toDiffTime #-}
toDiffTime :: Data_IntervalDS -> DiffTime
toDiffTime Data_IntervalDS{..} = picosecondsToDiffTime $ toInteger fseconds + (10^12) * (toInteger seconds + 60 * (toInteger minutes + 60 * (toInteger hours + 24 * toInteger days)))

{-# INLINE toDiffTime' #-}
toDiffTime' :: Data_IntervalYM -> DiffTime
toDiffTime' Data_IntervalYM{..} = secondsToDiffTime $ 30 * 86400 * 10^12 * (toInteger years * 12 + toInteger months)

{-# INLINE fromUTCTime #-}
fromUTCTime :: UTCTime -> Data_Timestamp
fromUTCTime UTCTime{..} =
  let (year,month,day)    = toGregorian utctDay
      Data_IntervalDS{..} = fromDiffTime utctDayTime
  in Data_Timestamp (fromInteger year) (fe month) (fe day) (fe hours) (fe minutes) (fe seconds) (fe fseconds) 0 0

{-# INLINE fe #-}
fe :: (Integral a, Num b) => a -> b
fe = fromInteger . toInteger

{-# INLINE toUTCTime #-}
toUTCTime :: Data_Timestamp -> UTCTime
toUTCTime = zonedTimeToUTC . toZonedTime False

{-# INLINE toUTCTimeL #-}
toUTCTimeL :: Data_Timestamp -> UTCTime
toUTCTimeL = zonedTimeToUTC . toZonedTime True

{-# INLINE fromZonedTime #-}
fromZonedTime :: ZonedTime -> Data_Timestamp
fromZonedTime ZonedTime{..} =
  let ts           = fromUTCTime (localTimeToUTC utc zonedTimeToLocalTime)
      TimeZone{..} = zonedTimeZone
      (h,m)        = timeZoneMinutes `divMod` 60
  in  ts { tzHourOffset = fe h, tzMinuteOffset = fe m }

{-# INLINE toZonedTime #-}
toZonedTime :: Bool -> Data_Timestamp -> ZonedTime
toZonedTime isLocal Data_Timestamp{..} =
  let utctDay     = fromGregorian (fe year) (fe month) (fe day)
      days        = 0
      hours       = fe hour
      minutes     = fe minute
      seconds     = fe second
      fseconds    = fe fsecond
      utctDayTime = toDiffTime Data_IntervalDS{..}
      offset      = 60 * (toInteger tzHourOffset * 60 + toInteger tzMinuteOffset)
      timezone    = minutesToTimeZone $ fromInteger offset
  in if isLocal
       then utcToZonedTime timezone $ addUTCTime (fromInteger $ negate offset) UTCTime{..}
       else utcToZonedTime timezone UTCTime{..}

{-# INLINE fromUTCTimeD #-}
fromUTCTimeD :: UTCTime -> CDouble
fromUTCTimeD = realToFrac . utcTimeToPOSIXSeconds

{-# INLINE toUTCTimeD #-}
toUTCTimeD :: CDouble -> UTCTime
toUTCTimeD = posixSecondsToUTCTime . realToFrac

{-# INLINE fromDouble #-}
fromDouble :: Double -> CDouble
fromDouble = CDouble

{-# INLINE toDouble #-}
toDouble :: CDouble -> Double
toDouble (CDouble c) = c

{-# INLINE fromFloat #-}
fromFloat :: Float -> CFloat
fromFloat = CFloat

{-# INLINE toFloat #-}
toFloat :: CFloat -> Float
toFloat (CFloat c) = c

{-# INLINE singleError #-}
singleError :: Text -> IO a
singleError name = throw $ DpiException $ "type mismatch " <> name


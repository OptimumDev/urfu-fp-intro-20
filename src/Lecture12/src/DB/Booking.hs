{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where

import Data.Aeson hiding (Success)
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics
import Control.Monad.IO.Class

import DB.MovieSession
import DB.Seat
import DB.Internal

{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  -- ^ этот инстанс позволяет использовать `BookingId` с функциями для базы данных
  -- `via` говорит о том, что `BookingId` нужно использовать как `Integer`.
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer
  -- ^ тоже самое для других классов

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , movieSessionId :: MovieSessionId
  , isPreliminary :: Bool
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

deriving instance FromRow Booking
deriving instance ToRow Booking
-- ^ получаем возможность записывать и читать данные из базы данных с помощью `Booking`

instance ToJSON Booking
instance FromJSON Booking
-- ^ возможность для работы с JSON

data BookingResult = Success | Expired | AlreadyPaid deriving (Eq, Show, Generic)

instance ToJSON BookingResult
instance FromJSON BookingResult

data BookingResponse = BookingSuccess
  { result :: BookingResult
  , bookingMovieSessionId :: MovieSessionId
  , bookingSeatId :: SeatId
  } | BookingFail { result :: BookingResult }
  deriving (Eq, Show, Generic)

instance ToJSON BookingResponse
instance FromJSON BookingResponse

bookingSuccess :: MovieSessionId -> SeatId -> BookingResponse
bookingSuccess = BookingSuccess Success

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}

getBooking :: DBMonad m => BookingId -> m [Booking]
getBooking bookingId = runSQL $ \conn ->
  query conn "SELECT id, seat_id, movie_session_id, is_preliminary, created_at FROM bookings WHERE id = ?" bookingId

checkoutBooking :: DBMonad m => BookingId -> m ()
checkoutBooking bookingId = runSQL $ \conn ->
  execute conn "UPDATE bookings SET is_preliminary = false WHERE id = ?" bookingId

deleteBooking :: DBMonad m => BookingId -> m ()
deleteBooking bookingId = runSQL $ \conn ->
  execute conn "DELETE FROM bookings WHERE id = ?" bookingId

isExpired :: UTCTime -> IO Bool
isExpired createdTime = do
  curTime <- getCurrentTime
  return $ addUTCTime 600 createdTime < curTime

tryBook :: DBMonad m => BookingId -> m (Maybe BookingResponse)
tryBook bookingId = do
  booking <- getBooking bookingId
  case booking of
    [] -> return Nothing
    Booking _ seat movie isPreliminary created : _ -> do
      expired <- liftIO $ isExpired created
      return $ Just $ case (isPreliminary, expired) of
        (False, _) -> BookingFail { result = AlreadyPaid }
        (_, True) -> BookingFail { result = Expired }
        _ -> bookingSuccess movie seat
    

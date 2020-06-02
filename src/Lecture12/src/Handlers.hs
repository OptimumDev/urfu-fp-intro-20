{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server

import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

checkout :: MonadIO m => BookingId -> AppT m BookingResponse
checkout bookingId = do
  booking <- tryBook bookingId
  case booking of
    Just f@(BookingFail Expired) -> do
      deleteBooking bookingId
      return f
    Just f@(BookingFail _) -> return f
    Just s@(BookingSuccess _ _ seatId) -> do
      checkoutBooking bookingId
      checkoutSeat seatId
      return s
    _ -> throwJSONError err404 $ JSONError "booking is not found"

refund :: MonadIO m => BookingId -> AppT m ()
refund bookingId = do
  booking <- getBooking bookingId
  case booking of
    [] -> throwJSONError err404 $ JSONError "booking is not found"
    Booking _ seatId _ _ _ : _ -> do
      deleteBooking bookingId
      refundSeat seatId

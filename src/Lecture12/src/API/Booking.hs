module API.Session where

import Servant.API
import DB.MovieSession
import DB.Seat
import DB.Booking

type BookingAPI
  = "api" :> "checkout" :> Capture "id" BookingId :> Get '[JSON] BookingResponse
    -- ^ метод для оплаты бронировния
  :<|>
    ("api" :> "refund" :> Capture "id" BookingId :> Get '[JSON] ())
    -- ^ метод для отмены бронировния
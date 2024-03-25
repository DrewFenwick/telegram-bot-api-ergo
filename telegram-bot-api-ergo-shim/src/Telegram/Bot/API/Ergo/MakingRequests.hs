module Telegram.Bot.API.Ergo.MakingRequests where
import qualified Telegram.Bot.API.MakingRequests as Old

mapResponse :: (a -> b) -> Old.Response a -> Old.Response b
mapResponse f response = response {Old.responseResult = f response.responseResult} 
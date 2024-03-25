module Telegram.Bot.API.Ergo.GettingUpdates (getUpdates)
where

import Servant.Client
import Telegram.Bot.API qualified as Old (Response)
import Telegram.Bot.API.GettingUpdates qualified as Old

import Telegram.Bot.API.Ergo.Types.Update
import Telegram.Bot.API.Ergo.HasErgoRep
import Telegram.Bot.API.Ergo.MakingRequests (mapResponse)

getUpdates :: GetUpdatesRequest -> ClientM (Old.Response [Update])
getUpdates = fmap (mapResponse (fmap unsafeFromOldRep)) . Old.getUpdates . toOldRep
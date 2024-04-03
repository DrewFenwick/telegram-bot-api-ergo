module Telegram.Bot.API.Ergo.Types.Int where
import Data.Int (Int64)

-- | The largest fixed-precision Integer that can contain all integers used by Telegram's API
--
-- While many Telegram integers can be represented by 32 bits, many unique identifier numbers have a maximum of 52 significant bits,
-- so Int64 suffices to contain both.

type TelegramInt = Int64

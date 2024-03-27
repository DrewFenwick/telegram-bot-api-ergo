{-# LANGUAGE StrictData #-}

module Telegram.Bot.API.Ergo.Types.Update (
  UpdateId (..),
  Update (..),
  UpdateEvent (..),
  UpdateType (..),
  GetUpdatesRequest (..),
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Int (Int64)
import Data.Vector (Vector)
import GHC.Generics
import Telegram.Bot.API qualified as Old
import Telegram.Bot.API.Ergo.Types.Internal.Utils
import Telegram.Bot.API.Internal.Utils

newtype UpdateId = UpdateId Int64
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data Update = Update {updateId :: !UpdateId, updateEvent :: !UpdateEvent}
  deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = withObject "Update" \o ->
    Update
      <$> o .: "update_id"
      <*> genericParseJSON eventOptions (Object $ Aeson.delete "update_id" o)

instance ToJSON Update where
  toJSON update = case toJSON update.updateEvent of
    Object o -> Object (Aeson.insert "update_id" (toJSON update.updateId) o)
    _ -> error "the impossible happened: UpdateEvents should always decode to an Object, but apparently they don't!"

data UpdateEvent
  = -- | New incoming message of any kind — text, photo, sticker, etc.
    EventMessage Old.Message
  | -- | New version of a message that is known to the bot and was edited
    EventEditedMessage Old.Message
  | -- | New incoming channel post of any kind — text, photo, sticker, etc.
    EventChannelPost Old.Message
  | -- | A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify "message_reaction" in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
    EventMessageReaction Old.MessageReactionUpdated
  | -- | Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify "message_reaction_count" in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
    EventMessageReactionCount Old.MessageReactionCountUpdated
  | -- | New version of a channel post that is known to the bot and was edited
    EventEditedChannelPost Old.Message
  | -- | New incoming inline query
    EventInlineQuery Old.InlineQuery
  | -- | The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
    EventChosenInlineResult Old.ChosenInlineResult
  | -- | New incoming callback query
    EventCallbackQuery Old.CallbackQuery
  | -- | New incoming shipping query. Only for invoices with flexible price
    EventShippingQuery Old.ShippingQuery
  | -- | New incoming pre-checkout query. Contains full information about checkout
    EventPreCheckoutQuery Old.PreCheckoutQuery
  | -- | New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
    EventPoll Old.Poll
  | -- | A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
    EventPollAnswer Old.PollAnswer
  | -- | The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
    EventMyChatMember Old.ChatMemberUpdated
  | -- | A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
    EventChatMember Old.ChatMemberUpdated
  | -- | A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
    EventChatJoinRequest Old.ChatJoinRequest
  | -- | A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
    EventChatBoost Old.ChatBoostUpdated
  | -- | A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
    EventRemovedChatBoost Old.ChatBoostRemoved
  deriving (Generic, Show)

instance FromJSON UpdateEvent where
  parseJSON = genericParseJSON eventOptions

instance ToJSON UpdateEvent where
  toJSON = genericToJSON eventOptions
  toEncoding = genericToEncoding eventOptions

eventOptions :: Options
eventOptions = (jsonOptions "Event"){sumEncoding = ObjectWithSingleField}

data UpdateType
  = -- \^ Update types for the purposes of requesting specific update types.

    -- | New incoming message of any kind — text, photo, sticker, etc.
    UpdateMessage
  | -- | New version of a message that is known to the bot and was edited
    UpdateEditedMessage
  | -- | New incoming channel post of any kind — text, photo, sticker, etc.
    UpdateChannelPost
  | -- | A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify "message_reaction" in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
    UpdateMessageReaction
  | -- | Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify "message_reaction_count" in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
    UpdateMessageReactionCount
  | -- | New version of a channel post that is known to the bot and was edited
    UpdateEditedChannelPost
  | -- | New incoming inline query
    UpdateInlineQuery
  | -- | The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
    UpdateChosenInlineResult
  | -- | New incoming callback query
    UpdateCallbackQuery
  | -- | New incoming shipping query. Only for invoices with flexible price
    UpdateShippingQuery
  | -- | New incoming pre-checkout query. Contains full information about checkout
    UpdatePreCheckoutQuery
  | -- | New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
    UpdatePoll
  | -- | A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
    UpdatePollAnswer
  | -- | The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
    UpdateMyChatMember
  | -- | A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
    UpdateChatMember
  | -- | A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
    UpdateChatJoinRequest
  | -- | A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
    UpdateChatBoost
  | -- | A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
    UpdateRemovedChatBoost
  deriving (Eq, Enum, Bounded, Show, Generic)
  deriving (ToJSON, FromJSON) via DefGenericEncoding UpdateType

data GetUpdatesRequest = GetUpdatesRequest
  { getUpdatesOffset :: Maybe UpdateId
  -- ^ Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten.
  , getUpdatesLimit :: Maybe Int
  -- ^ Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100.
  , getUpdatesTimeout :: Maybe Old.Seconds
  -- ^ Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.
  , getUpdatesAllowedUpdates :: Maybe (Vector UpdateType)
  -- ^ List the types of updates you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See GetUpdates for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used. Please note that this parameter doesn't affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time.
  }
  deriving (Generic)
  deriving (ToJSON, FromJSON) via DefGenericEncoding GetUpdatesRequest

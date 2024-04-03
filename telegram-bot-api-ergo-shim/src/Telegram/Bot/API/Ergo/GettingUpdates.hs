{-| 
This module is the @telegram-bot-api-ergo-shim@ version of [Telegram.Bot.API.GettingUpdates](https://hackage.haskell.org/package/telegram-bot-api-7.0/docs/Telegram-Bot-API-GettingUpdates.html).
It provides a variation on the interface that uses the more ergonomic types provided by @telegram-bot-api-ergo@.

This module corresponds to the [Getting Updates](https://core.telegram.org/bots/api#getting-updates)
section of the Telegram API documentation, excluding webhook functionality,
which is provided [Telegram.Bot.API.Webhook](https://hackage.haskell.org/package/telegram-bot-api-7.0/docs/Telegram-Bot-API-Webhook.html).
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.API.Ergo.GettingUpdates (
  module Telegram.Bot.API.Ergo.Types.Update,

  -- * Update
  UpdateId(..),
  Update(..),
  UpdateEvent(..),

  -- * Getting Updates
  GetUpdates,
  GetUpdatesRequest(..),
  getUpdates,
  UpdateType(..),
  defGetUpdates,

  -- * Compatability Functions
  -- | It is not recommended that you use the functions from this section in new code.
  -- 
  -- These functions are provided merely to reduce the amount of breakage when using this module as an alternative to [Telegram.Bot.API.GettingUpdates](https://hackage.haskell.org/package/telegram-bot-api-7.0/docs/Telegram-Bot-API-GettingUpdates.html);
  -- Hence, naming conventions follow those of that module.

  -- ** Old-style update accessors
  --
  -- It is recommended that you use pattern matching on `Update` and `UpdateEvent`, or optics instead of these functions.

  updateUpdateId,
  updateMessage,
  updateEditedMessage,
  updateChannelPost,
  updateMessageReaction,
  updateMessageReactionCount,
  updateEditedChannelPost,
  updateInlineQuery,
  updateChosenInlineResult,
  updateCallbackQuery,
  updateShippingQuery,
  updatePreCheckoutQuery,
  updatePoll,
  updatePollAnswer,
  updateMyChatMember,
  updateChatMember,
  updateChatJoinRequest,
  updateChatBoost,
  updateRemovedChatBoost,

  -- ** Pseudo-accessors

  extractUpdateMessage,
  updateChatId,
  getUpdatesOffset,
  getUpdatesLimit,
  getUpdatesTimeout,
  getUpdatesAllowedUpdates,
)
where

import Servant.Client hiding (Response)
import Telegram.Bot.API qualified as Old

import Data.Proxy
import Servant.API
import Telegram.Bot.API.Ergo.Types.Update
import Telegram.Bot.API.Internal.TH (makeDefault)
import Data.Vector (Vector)

-- | The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
--
--  The suggested alternative to using this function is to either pattern match 
updateUpdateId ::
  Update ->
  UpdateId
updateUpdateId = (.updateId)

-- | New incoming message of any kind — text, photo, sticker, etc.
updateMessage ::
  Update ->
  Maybe Old.Message
updateMessage Update{event = EventMessage msg} = Just msg
updateMessage _ = Nothing

-- | New version of a message that is known to the bot and was edited
updateEditedMessage ::
  Update ->
  Maybe Old.Message
updateEditedMessage Update{event = EventEditedMessage msg} = Just msg
updateEditedMessage _ = Nothing

-- | New incoming channel post of any kind — text, photo, sticker, etc.
updateChannelPost ::
  Update ->
  Maybe Old.Message
updateChannelPost Update{event = EventChannelPost msg} = Just msg
updateChannelPost _ = Nothing

-- | A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify "message_reaction" in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
updateMessageReaction ::
  Update ->
  Maybe Old.MessageReactionUpdated
updateMessageReaction Update{event = EventMessageReaction msg} = Just msg
updateMessageReaction _ = Nothing

-- | Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify "message_reaction_count" in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
updateMessageReactionCount ::
  Update ->
  Maybe Old.MessageReactionCountUpdated
updateMessageReactionCount Update{event = EventMessageReactionCount countUpdate} = Just countUpdate
updateMessageReactionCount _ = Nothing

-- | New version of a channel post that is known to the bot and was edited
updateEditedChannelPost ::
  Update ->
  Maybe Old.Message
updateEditedChannelPost Update{event = EventChannelPost msg} = Just msg
updateEditedChannelPost _ = Nothing

-- | New incoming inline query
updateInlineQuery ::
  Update ->
  Maybe Old.InlineQuery
updateInlineQuery Update{event = EventInlineQuery iq} = Just iq
updateInlineQuery _ = Nothing

-- | The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
updateChosenInlineResult ::
  Update ->
  Maybe Old.ChosenInlineResult
updateChosenInlineResult Update{event = EventChosenInlineResult x} = Just x
updateChosenInlineResult _ = Nothing

-- | New incoming callback query
updateCallbackQuery ::
  Update ->
  Maybe Old.CallbackQuery
updateCallbackQuery Update{event = EventCallbackQuery cq} = Just cq
updateCallbackQuery _ = Nothing

-- | New incoming shipping query. Only for invoices with flexible price
updateShippingQuery ::
  Update ->
  Maybe Old.ShippingQuery
updateShippingQuery Update{event = EventShippingQuery x} = Just x
updateShippingQuery _ = Nothing

-- | New incoming pre-checkout query. Contains full information about checkout
updatePreCheckoutQuery ::
  Update ->
  Maybe Old.PreCheckoutQuery
updatePreCheckoutQuery Update{event = EventPreCheckoutQuery x} = Just x
updatePreCheckoutQuery _ = Nothing

-- | New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
updatePoll ::
  Update ->
  Maybe Old.Poll
updatePoll Update{event = EventPoll x} = Just x
updatePoll _ = Nothing

-- | A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
updatePollAnswer ::
  Update ->
  Maybe Old.PollAnswer
updatePollAnswer Update{event = EventPollAnswer x} = Just x
updatePollAnswer _ = Nothing

-- | The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
updateMyChatMember ::
  Update ->
  Maybe Old.ChatMemberUpdated
updateMyChatMember Update{event = EventMyChatMember x} = Just x
updateMyChatMember _ = Nothing

-- | A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
updateChatMember ::
  Update ->
  Maybe Old.ChatMemberUpdated
updateChatMember Update{event = EventChatMember x} = Just x
updateChatMember _ = Nothing

-- | A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
updateChatJoinRequest ::
  Update ->
  Maybe Old.ChatJoinRequest
updateChatJoinRequest Update{event = EventChatJoinRequest x} = Just x
updateChatJoinRequest _ = Nothing

-- | A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
updateChatBoost ::
  Update ->
  Maybe Old.ChatBoostUpdated
updateChatBoost Update{event = EventChatBoost x} = Just x
updateChatBoost _ = Nothing

-- | A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
updateRemovedChatBoost ::
  Update ->
  Maybe Old.ChatBoostRemoved
updateRemovedChatBoost Update{event = EventRemovedChatBoost x} = Just x
updateRemovedChatBoost _ = Nothing

-- | If the update relates to a message, this function will extract the chat ID.
updateChatId :: Update -> Maybe Old.ChatId
updateChatId = fmap ((.chatId) . (.messageChat)) . extractUpdateMessage

-- | If the update relates to a message, this function will extract the message.
extractUpdateMessage :: Update -> Maybe Old.Message
extractUpdateMessage update = case update.event of
  EventMessage x -> Just x
  EventEditedMessage x -> Just x
  EventChannelPost x -> Just x
  EventEditedChannelPost x -> Just x
  EventCallbackQuery cq -> cq.callbackQueryMessage
  _ -> Nothing

-- | A Servant description of the getUpdates endpoint.
type GetUpdates =
  "getUpdates" :> ReqBody '[JSON] GetUpdatesRequest :> Get '[JSON] (Old.Response [Update])

-- | Use this method to receive incoming updates using long polling.
-- An list of 'Update' objects is returned.
--
-- NOTE: This method will not work if an outgoing webhook is set up.
--
-- NOTE: In order to avoid getting duplicate updates, recalculate offset after each server response.
getUpdates :: GetUpdatesRequest -> ClientM (Old.Response [Update])
getUpdates = client (Proxy @GetUpdates)

-- getUpdates :: GetUpdatesRequest -> ClientM (Old.Response [Update])
-- getUpdates = fmap (mapResponse (fmap unsafeFromOldRep)) . Old.getUpdates . toOldRep

-- | A default update poll request with all optional fields unset.
makeDefault ''GetUpdatesRequest

-- | Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten.
getUpdatesOffset :: GetUpdatesRequest -> Maybe UpdateId
getUpdatesOffset = (.offset)


-- | Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100.
getUpdatesLimit :: GetUpdatesRequest -> Maybe Int
getUpdatesLimit = (.limit)

-- | Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.
getUpdatesTimeout :: GetUpdatesRequest -> Maybe Old.Seconds
getUpdatesTimeout = (.timeout)

-- | List the types of updates you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See GetUpdates for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used. Please note that this parameter doesn't affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time.
getUpdatesAllowedUpdates :: GetUpdatesRequest -> Maybe (Vector UpdateType)
getUpdatesAllowedUpdates = (.allowedUpdates)
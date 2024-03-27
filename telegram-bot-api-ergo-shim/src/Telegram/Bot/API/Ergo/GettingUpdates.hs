{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.API.Ergo.GettingUpdates (
  module Telegram.Bot.API.Ergo.Types.Update,
  getUpdates,
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
  extractUpdateMessage,
  updateChatId,
  defGetUpdates,
)
where

import Servant.Client hiding (Response)
import Telegram.Bot.API qualified as Old

import Data.Proxy
import Servant.API
import Telegram.Bot.API.Ergo.Types.Update
import Telegram.Bot.API.Internal.TH (makeDefault)

updateUpdateId ::
  Update ->
  -- | The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
  UpdateId
updateUpdateId update = update.updateId

updateMessage ::
  Update ->
  -- | New incoming message of any kind — text, photo, sticker, etc.
  Maybe Old.Message
updateMessage Update{updateEvent = EventMessage msg} = Just msg
updateMessage _ = Nothing

updateEditedMessage ::
  Update ->
  -- | New version of a message that is known to the bot and was edited
  Maybe Old.Message
updateEditedMessage Update{updateEvent = EventEditedMessage msg} = Just msg
updateEditedMessage _ = Nothing

updateChannelPost ::
  Update ->
  -- | New incoming channel post of any kind — text, photo, sticker, etc.
  Maybe Old.Message
updateChannelPost Update{updateEvent = EventChannelPost msg} = Just msg
updateChannelPost _ = Nothing

updateMessageReaction ::
  Update ->
  -- | A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify "message_reaction" in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
  Maybe Old.MessageReactionUpdated
updateMessageReaction Update{updateEvent = EventMessageReaction msg} = Just msg
updateMessageReaction _ = Nothing

updateMessageReactionCount ::
  Update ->
  -- | Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify "message_reaction_count" in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
  Maybe Old.MessageReactionCountUpdated
updateMessageReactionCount Update{updateEvent = EventMessageReactionCount countUpdate} = Just countUpdate
updateMessageReactionCount _ = Nothing

updateEditedChannelPost ::
  Update ->
  -- | New version of a channel post that is known to the bot and was edited
  Maybe Old.Message
updateEditedChannelPost Update{updateEvent = EventChannelPost msg} = Just msg
updateEditedChannelPost _ = Nothing

updateInlineQuery ::
  Update ->
  -- | New incoming inline query
  Maybe Old.InlineQuery
updateInlineQuery Update{updateEvent = EventInlineQuery iq} = Just iq
updateInlineQuery _ = Nothing

updateChosenInlineResult ::
  Update ->
  -- | The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
  Maybe Old.ChosenInlineResult
updateChosenInlineResult Update{updateEvent = EventChosenInlineResult x} = Just x
updateChosenInlineResult _ = Nothing

updateCallbackQuery ::
  Update ->
  -- | New incoming callback query
  Maybe Old.CallbackQuery
updateCallbackQuery Update{updateEvent = EventCallbackQuery cq} = Just cq
updateCallbackQuery _ = Nothing

updateShippingQuery ::
  Update ->
  -- | New incoming shipping query. Only for invoices with flexible price
  Maybe Old.ShippingQuery
updateShippingQuery Update{updateEvent = EventShippingQuery x} = Just x
updateShippingQuery _ = Nothing

updatePreCheckoutQuery ::
  Update ->
  -- | New incoming pre-checkout query. Contains full information about checkout
  Maybe Old.PreCheckoutQuery
updatePreCheckoutQuery Update{updateEvent = EventPreCheckoutQuery x} = Just x
updatePreCheckoutQuery _ = Nothing

updatePoll ::
  Update ->
  -- | New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
  Maybe Old.Poll
updatePoll Update{updateEvent = EventPoll x} = Just x
updatePoll _ = Nothing

updatePollAnswer ::
  Update ->
  -- | A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
  Maybe Old.PollAnswer
updatePollAnswer Update{updateEvent = EventPollAnswer x} = Just x
updatePollAnswer _ = Nothing

updateMyChatMember ::
  Update ->
  -- | The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
  Maybe Old.ChatMemberUpdated
updateMyChatMember Update{updateEvent = EventMyChatMember x} = Just x
updateMyChatMember _ = Nothing

updateChatMember ::
  Update ->
  -- | A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
  Maybe Old.ChatMemberUpdated
updateChatMember Update{updateEvent = EventChatMember x} = Just x
updateChatMember _ = Nothing

updateChatJoinRequest ::
  Update ->
  -- | A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
  Maybe Old.ChatJoinRequest
updateChatJoinRequest Update{updateEvent = EventChatJoinRequest x} = Just x
updateChatJoinRequest _ = Nothing

updateChatBoost ::
  Update ->
  -- | A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
  Maybe Old.ChatBoostUpdated
updateChatBoost Update{updateEvent = EventChatBoost x} = Just x
updateChatBoost _ = Nothing

updateRemovedChatBoost ::
  Update ->
  -- | A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
  Maybe Old.ChatBoostRemoved
updateRemovedChatBoost Update{updateEvent = EventRemovedChatBoost x} = Just x
updateRemovedChatBoost _ = Nothing

updateChatId :: Update -> Maybe Old.ChatId
updateChatId = fmap ((.chatId) . (.messageChat)) . extractUpdateMessage

extractUpdateMessage :: Update -> Maybe Old.Message
extractUpdateMessage update = case update.updateEvent of
  EventMessage x -> Just x
  EventEditedMessage x -> Just x
  EventChannelPost x -> Just x
  EventEditedChannelPost x -> Just x
  EventCallbackQuery cq -> cq.callbackQueryMessage
  _ -> Nothing

type GetUpdates =
  "getUpdates" :> ReqBody '[JSON] GetUpdatesRequest :> Get '[JSON] (Old.Response [Update])

getUpdates :: GetUpdatesRequest -> ClientM (Old.Response [Update])
getUpdates = client (Proxy @GetUpdates)

-- getUpdates :: GetUpdatesRequest -> ClientM (Old.Response [Update])
-- getUpdates = fmap (mapResponse (fmap unsafeFromOldRep)) . Old.getUpdates . toOldRep

makeDefault ''GetUpdatesRequest
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.API.Ergo.HasErgoRep (HasOldRep (..), unsafeFromOldRep) where

import Control.Applicative
import Data.Vector qualified as V
import Telegram.Bot.API qualified as Old
import Telegram.Bot.API.Ergo.Types

type ToErgoRepError = String

class HasOldRep a where
  type OldRep a
  fromOldRep :: OldRep a -> Either ToErgoRepError a
  toOldRep :: a -> OldRep a
  {-# MINIMAL fromOldRep, toOldRep #-}

unsafeFromOldRep :: HasOldRep a => OldRep a -> a
unsafeFromOldRep = either error id . fromOldRep

instance HasOldRep UpdateId where
  type OldRep UpdateId = Old.UpdateId
  fromOldRep (Old.UpdateId i) = pure . UpdateId . fromIntegral $ i
  toOldRep (UpdateId i) = Old.UpdateId $ fromIntegral i

instance HasOldRep Update where
  type OldRep Update = Old.Update
  fromOldRep update =
    case getOldUpdateEvent update of
      Nothing -> Left failureMsg
      Just ue -> Right $ Update 
        { updateId = update.updateUpdateId
        , updateEvent = ue
        }
    where
      failureMsg = "Extracting an update event from telegram-bot-api's Update type failed.\n Please contact the maintainer of telegram-bot-api-ergo-shim."
  
  toOldRep update = case update.updateEvent of
    EventMessage              x -> eventlessUpdate{Old.updateMessage = Just x}
    EventEditedMessage        x -> eventlessUpdate{Old.updateEditedMessage = Just x}
    EventChannelPost          x -> eventlessUpdate{Old.updateChannelPost = Just x}
    EventMessageReaction      x -> eventlessUpdate{Old.updateMessageReaction = Just x}
    EventMessageReactionCount x -> eventlessUpdate{Old.updateMessageReactionCount = Just x}
    EventEditedChannelPost    x -> eventlessUpdate{Old.updateEditedChannelPost = Just x}
    EventInlineQuery          x -> eventlessUpdate{Old.updateInlineQuery = Just x}
    EventChosenInlineResult   x -> eventlessUpdate{Old.updateChosenInlineResult = Just x}
    EventCallbackQuery        x -> eventlessUpdate{Old.updateCallbackQuery = Just x}
    EventShippingQuery        x -> eventlessUpdate{Old.updateShippingQuery = Just x}
    EventPreCheckoutQuery     x -> eventlessUpdate{Old.updatePreCheckoutQuery = Just x}
    EventPoll                 x -> eventlessUpdate{Old.updatePoll = Just x}
    EventPollAnswer           x -> eventlessUpdate{Old.updatePollAnswer = Just x}
    EventMyChatMember         x -> eventlessUpdate{Old.updateMyChatMember = Just x}
    EventChatMember           x -> eventlessUpdate{Old.updateChatMember = Just x}
    EventChatJoinRequest      x -> eventlessUpdate{Old.updateChatJoinRequest = Just x}
    EventChatBoost            x -> eventlessUpdate{Old.updateChatBoost = Just x}
    EventRemovedChatBoost     x -> eventlessUpdate{Old.updateRemovedChatBoost = Just x}
    where 
      eventlessUpdate = Old.Update 
        { Old.updateUpdateId = update.updateId
        , Old.updateMessage = Nothing
        , Old.updateEditedMessage = Nothing
        , Old.updateChannelPost = Nothing
        , Old.updateMessageReaction = Nothing
        , Old.updateMessageReactionCount = Nothing
        , Old.updateEditedChannelPost = Nothing
        , Old.updateInlineQuery = Nothing
        , Old.updateChosenInlineResult = Nothing
        , Old.updateCallbackQuery = Nothing
        , Old.updateShippingQuery = Nothing
        , Old.updatePreCheckoutQuery = Nothing
        , Old.updatePoll = Nothing
        , Old.updatePollAnswer = Nothing
        , Old.updateMyChatMember = Nothing
        , Old.updateChatMember = Nothing
        , Old.updateChatJoinRequest = Nothing
        , Old.updateChatBoost = Nothing
        , Old.updateRemovedChatBoost = Nothing
        } 

getOldUpdateEvent :: Old.Update -> Maybe UpdateEvent
getOldUpdateEvent Old.Update{..} =
  asum
    [ EventMessage <$> updateMessage
    , EventEditedMessage <$> updateEditedMessage
    , EventChannelPost <$> updateChannelPost
    , EventMessageReaction <$> updateMessageReaction
    , EventMessageReactionCount <$> updateMessageReactionCount
    , EventEditedChannelPost <$> updateEditedChannelPost
    , EventInlineQuery <$> updateInlineQuery
    , EventChosenInlineResult <$> updateChosenInlineResult
    , EventCallbackQuery <$> updateCallbackQuery
    , EventShippingQuery <$> updateShippingQuery
    , EventPreCheckoutQuery <$> updatePreCheckoutQuery
    , EventPoll <$> updatePoll
    , EventPollAnswer <$> updatePollAnswer
    , EventMyChatMember <$> updateMyChatMember
    , EventChatMember <$> updateChatMember
    , EventChatJoinRequest <$> updateChatJoinRequest
    , EventChatBoost <$> updateChatBoost
    , EventRemovedChatBoost <$> updateRemovedChatBoost
    ]
  
instance HasOldRep UpdateType where
  type OldRep UpdateType = Old.UpdateType
  fromOldRep = \case
    Old.UpdateMessage -> pure UpdateMessage
    Old.UpdateEditedMessage -> pure UpdateEditedMessage 
    Old.UpdateChannelPost -> pure UpdateChannelPost
    Old.UpdateEditedChannelPost -> pure UpdateEditedChannelPost
    Old.UpdateInlineQuery -> pure UpdateInlineQuery
    Old.UpdateCallbackQuery -> pure UpdateCallbackQuery
    Old.UpdateShippingQuery -> pure UpdateShippingQuery
    Old.UpdatePreCheckoutQuery -> pure UpdatePreCheckoutQuery
    _ -> Left "Could not parse telegram-bot-api UpdateType to new UpdateType"
    -- _ -> pure UpdateMessageReaction
    -- _ -> pure UpdateMessageReactionCount
    -- _ -> pure UpdateChosenInlineResult
    -- _ -> pure UpdatePoll
    -- _ -> pure UpdatePollAnswer
    -- _ -> pure UpdateMyChatMember
    -- _ -> pure UpdateChatMember
    -- _ -> pure UpdateChatJoinRequest
    -- _ -> pure UpdateChatBoost
    -- _ -> pure UpdateRemovedChatBoost
  toOldRep updateType = case updateType of
      UpdateMessage -> Old.UpdateMessage
      UpdateEditedMessage -> Old.UpdateEditedMessage 
      UpdateChannelPost -> Old.UpdateChannelPost
      UpdateEditedChannelPost -> Old.UpdateEditedChannelPost
      UpdateInlineQuery -> Old.UpdateInlineQuery
      UpdateCallbackQuery -> Old.UpdateCallbackQuery
      UpdateShippingQuery -> Old.UpdateShippingQuery
      UpdatePreCheckoutQuery -> Old.UpdatePreCheckoutQuery
      UpdateMessageReaction ->  failure
      UpdateMessageReactionCount -> failure
      UpdateChosenInlineResult -> failure
      UpdatePoll -> failure
      UpdatePollAnswer -> failure
      UpdateMyChatMember -> failure
      UpdateChatMember -> failure
      UpdateChatJoinRequest -> failure
      UpdateChatBoost -> failure
      UpdateRemovedChatBoost -> failure
    where 
      failure = error $ "Couldn't convert " ++ show updateType ++ " to telegram-bot-api UpdateType value because it doesn't implement that update type yet."

    -- UpdateMessageReaction -> Old.UpdateMessageReaction
    -- UpdateMessageReactionCount -> Old.UpdateMessageReactionCount
    -- UpdateChosenInlineResult -> Old.UpdateChosenInlineResult
    -- UpdatePoll -> Old.UpdatePoll
    -- UpdatePollAnswer -> Old.UpdatePollAnswer
    -- UpdateMyChatMember -> Old.UpdateMyChatMember
    -- UpdateChatMember -> Old.UpdateChatMember
    -- UpdateChatJoinRequest -> Old.UpdateChatJoinRequest
    -- UpdateChatBoost -> Old.UpdateChatBoost
    -- UpdateRemovedChatBoost -> Old.UpdateRemovedChatBoost



instance HasOldRep GetUpdatesRequest where
  type OldRep GetUpdatesRequest = Old.GetUpdatesRequest
  fromOldRep Old.GetUpdatesRequest {..} = do
    getUpdatesOffset <- traverse fromOldRep getUpdatesOffset
    getUpdatesAllowedUpdates <- traverse (traverse fromOldRep . V.fromList) getUpdatesAllowedUpdates
    pure $ GetUpdatesRequest {..}
  toOldRep GetUpdatesRequest {..} = 
    Old.GetUpdatesRequest { getUpdatesOffset = toOldRep <$> getUpdatesOffset
                          , getUpdatesAllowedUpdates = V.toList . fmap toOldRep <$> getUpdatesAllowedUpdates
                          , ..
                          }
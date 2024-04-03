{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Telegram.Bot.API.Ergo.HasOldRep (HasOldRep (..), unsafeFromOldRep) where

import Control.Applicative ( asum )
import Data.Int (Int64)
import Data.Vector qualified as V
import Telegram.Bot.API qualified as Old
import Telegram.Bot.API.Ergo.Types
    ( GetUpdatesRequest(..),
      Update(..),
      UpdateEvent(..),
      UpdateId(..),
      UpdateType(..) )
import Telegram.Bot.API.Internal.TH ( makeDefault )

makeDefault ''Old.Update

defOldUpdate :: Old.UpdateId -> Old.Update
defOldUpdate = defUpdate

type ToErgoRepError = String

class HasOldRep a where
  type OldRep a
  fromOldRep :: OldRep a -> Either ToErgoRepError a
  toOldRep :: a -> OldRep a
  {-# MINIMAL fromOldRep, toOldRep #-}

unsafeFromOldRep :: (HasOldRep a) => OldRep a -> a
unsafeFromOldRep = either error id . fromOldRep

instance HasOldRep UpdateId where
  type OldRep UpdateId = Old.UpdateId
  fromOldRep (Old.UpdateId i) =
    if int64ContainsInt || i <= fromIntegral (maxBound :: Int64)
      then Right $ UpdateId (fromIntegral i)
      else Left "The telegram-bot-api UpdateId was too large to be a valid Telegram Update Id. Update Ids should be 64 bit integers."
  toOldRep (UpdateId i)
    | intContainsInt64 || i <= fromIntegral (maxBound :: Int) = Old.UpdateId $ fromIntegral i
    | otherwise = error "Telegram update IDs require up to 64 bit integers to store. telegram-bot-api uses Int to store them, but your machine doesn't have large enough machine integers. This led to a failure to convert a 64 bit telegram-bot-ergo-types UpdateId to a telegram-bot-api UpdateId because the ID was too large."

intContainsInt64 :: Bool
intContainsInt64 = toInteger (maxBound :: Int) >= toInteger (maxBound :: Int64)

int64ContainsInt :: Bool
int64ContainsInt = toInteger (maxBound :: Int) <= toInteger (maxBound :: Int64)

instance HasOldRep Update where
  type OldRep Update = Old.Update
  fromOldRep update = do
    updateId <- fromOldRep update.updateUpdateId
    event <- maybe (Left failureMsg) Right $ getOldUpdateEvent update
    pure Update{..}
   where
    failureMsg = "Extracting an update event from telegram-bot-api's Update type failed.\n Please contact the maintainer of telegram-bot-api-ergo-shim."

  toOldRep update = case update.event of
    EventMessage x -> eventlessUpdate{Old.updateMessage = Just x}
    EventEditedMessage x -> eventlessUpdate{Old.updateEditedMessage = Just x}
    EventChannelPost x -> eventlessUpdate{Old.updateChannelPost = Just x}
    EventMessageReaction x -> eventlessUpdate{Old.updateMessageReaction = Just x}
    EventMessageReactionCount x -> eventlessUpdate{Old.updateMessageReactionCount = Just x}
    EventEditedChannelPost x -> eventlessUpdate{Old.updateEditedChannelPost = Just x}
    EventInlineQuery x -> eventlessUpdate{Old.updateInlineQuery = Just x}
    EventChosenInlineResult x -> eventlessUpdate{Old.updateChosenInlineResult = Just x}
    EventCallbackQuery x -> eventlessUpdate{Old.updateCallbackQuery = Just x}
    EventShippingQuery x -> eventlessUpdate{Old.updateShippingQuery = Just x}
    EventPreCheckoutQuery x -> eventlessUpdate{Old.updatePreCheckoutQuery = Just x}
    EventPoll x -> eventlessUpdate{Old.updatePoll = Just x}
    EventPollAnswer x -> eventlessUpdate{Old.updatePollAnswer = Just x}
    EventMyChatMember x -> eventlessUpdate{Old.updateMyChatMember = Just x}
    EventChatMember x -> eventlessUpdate{Old.updateChatMember = Just x}
    EventChatJoinRequest x -> eventlessUpdate{Old.updateChatJoinRequest = Just x}
    EventChatBoost x -> eventlessUpdate{Old.updateChatBoost = Just x}
    EventRemovedChatBoost x -> eventlessUpdate{Old.updateRemovedChatBoost = Just x}
   where
    eventlessUpdate = defOldUpdate (toOldRep update.updateId)

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
    UpdateMessageReaction -> failure
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
  fromOldRep Old.GetUpdatesRequest{..} = do
    offset <- traverse fromOldRep getUpdatesOffset
    allowedUpdates <- traverse (traverse fromOldRep . V.fromList) getUpdatesAllowedUpdates
    pure $ GetUpdatesRequest{limit = getUpdatesLimit, timeout = getUpdatesTimeout, ..}
  toOldRep GetUpdatesRequest{..} =
    Old.GetUpdatesRequest
      { getUpdatesOffset = toOldRep <$> offset
      , getUpdatesAllowedUpdates = V.toList . fmap toOldRep <$> allowedUpdates
      , getUpdatesLimit = limit
      , getUpdatesTimeout = timeout
      }
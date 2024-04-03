{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Optics for @Telegram.Bot.API.Ergo.Types@
-}
module Telegram.Bot.API.Ergo.Types.Optics where

import Optics
import Telegram.Bot.API.Ergo.Types (
  GetUpdatesRequest,
  Update,
  UpdateEvent (..),
  UpdateId,
  UpdateType,
 )
import Telegram.Bot.API.Ergo.Types qualified as Ergo
import Telegram.Bot.API.Types (CallbackQuery (..), Message)

makeFieldLabelsNoPrefix ''Update

makeFieldLabelsNoPrefix ''GetUpdatesRequest

makePrismLabels ''UpdateId

makePrismLabels ''Update

makePrismLabels ''UpdateEvent

makePrismLabels ''UpdateType

makePrismLabels ''GetUpdatesRequest

-- | An affine traversal over the message an update event is about.
_eventMessage :: AffineTraversal' UpdateEvent Message
_eventMessage = atraversal f g
 where
  f event = case event of
    EventMessage msg -> Right msg
    EventEditedMessage msg -> Right msg
    EventChannelPost msg -> Right msg
    EventEditedChannelPost msg -> Right msg
    EventCallbackQuery cq -> maybe (Left event) Right cq.callbackQueryMessage
    _ -> Left event
  g event msg = case event of
    EventMessage _msg -> EventMessage msg
    EventEditedMessage _msg -> EventEditedMessage msg
    EventChannelPost _msg -> EventChannelPost msg
    EventEditedChannelPost _msg -> EventEditedChannelPost msg
    EventCallbackQuery cq -> EventCallbackQuery (set (#callbackQueryMessage % _Just) msg cq)
    _ -> event

-- | An affine traversal over the message an update is about.
_updateMessage :: AffineTraversal' Update Message
_updateMessage = #event % _eventMessage
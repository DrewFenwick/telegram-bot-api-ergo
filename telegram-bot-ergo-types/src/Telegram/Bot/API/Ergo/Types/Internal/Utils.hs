{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.API.Ergo.Types.Internal.Utils (
  NamableType (..),
  DefGenericEncoding (..),
  GenericallyEncodable,
  GenericallyDecodable,
  eventOptions,
  jsonOptions,
  standardDeriveJSON,
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Coerce (coerce)
import GHC.Generics
import Telegram.Bot.API.Internal.Utils hiding (jsonOptions)
import Data.Aeson.TH
import Language.Haskell.TH

class NamableType a where
  gDatatypeName :: String

instance (Rep a ~ D1 d f, Datatype d) => NamableType a where
  gDatatypeName = datatypeName @d undefined

type GenericallyDecodable a =
  (Generic a, GFromJSON Zero (Rep a), NamableType a)

type GenericallyEncodable a d f =
  (Generic a, GToJSON' Encoding Zero (Rep a), GToJSON Zero (Rep a), NamableType a)

instance (GenericallyEncodable a d f) => ToJSON (DefGenericEncoding a) where
  toJSON (DefGenericEncoding x) = genericToJSON (jsonOptions (gDatatypeName @a)) x
  toEncoding (DefGenericEncoding x) = genericToEncoding (jsonOptions (gDatatypeName @a)) x

instance (GenericallyDecodable a) => FromJSON (DefGenericEncoding a) where
  parseJSON = coerce (genericParseJSON (jsonOptions (gDatatypeName @a)) :: Value -> Parser a)

newtype DefGenericEncoding a = DefGenericEncoding a

eventOptions :: Options
eventOptions = (jsonOptions "Event"){sumEncoding = ObjectWithSingleField}

jsonOptions :: String -> Options
jsonOptions tname = defaultOptions
  { fieldLabelModifier     = wordsToSnake . camelWords 
  , constructorTagModifier = snakeFieldModifier tname
  , omitNothingFields      = True
  }

standardDeriveJSON :: Name -> Q [Dec]
standardDeriveJSON name = deriveJSON (jsonOptions (nameBase name)) name
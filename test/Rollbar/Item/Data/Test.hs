{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rollbar.Item.Data.Test where

import           Data.Aeson          (Value (Null, Object), decode', encode,
                                      toJSON)
import           Data.Text           (Text)

import           Rollbar.Item
import           Rollbar.QuickCheck  ()

import           Test.QuickCheck     (conjoin, quickCheck)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.Bifunctor (Bifunctor(first))

props :: IO ()
props =
    quickCheck $ conjoin
        [ prop_valueDataBodyHasRequiredKey
        , prop_encodingDataBodyHasRequiredKey
        ]

prop_valueDataBodyHasRequiredKey :: Data () '["Authorization"] -> Bool
prop_valueDataBodyHasRequiredKey x =
    length ms == 1 && fst (head ms) `elem` requiredBodyKeys
    where
    ms = key "body" (toJSON x)

prop_encodingDataBodyHasRequiredKey :: Data () '["Authorization"] -> Bool
prop_encodingDataBodyHasRequiredKey x =
    length ms == 1 && fst (head ms) `elem` requiredBodyKeys
    where
    ms = foldMap (key "body") (decode' $ encode x)

requiredBodyKeys :: [Text]
requiredBodyKeys = ["trace", "trace_chain", "message", "crash_report"]

key :: Text -> Value -> [(Text, Value)]
key k = \case
  Object o -> case KM.lookup (Key.fromText k) o of
    Just (Object o) -> first Key.toText <$> KM.toList o
    _        -> mempty
  _ -> mempty

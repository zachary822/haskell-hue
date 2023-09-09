{-# LANGUAGE DeriveGeneric #-}

module Lib.HueBridge where

import Data.Aeson
import Data.Text
import GHC.Generics

data BasicConfig = BasicConfig
  { name :: Text
  , datastoreversion :: Text
  , swversion :: Text
  , apiversion :: Text
  , mac :: Text
  , bridgeid :: Text
  , factorynew :: Bool
  , repalcebridgeid :: Maybe Text
  , modelid :: Text
  , starterkitid :: Text
  }
  deriving (Show, Generic)

instance FromJSON BasicConfig
instance ToJSON BasicConfig

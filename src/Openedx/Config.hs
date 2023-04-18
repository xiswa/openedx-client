{-# LANGUAGE DeriveGeneric #-}
module Openedx.Config (
    OpenedxConfig(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Servant.Client (BaseUrl)
import GHC.Generics
import Xiswa.Utils

data OpenedxConfig = OpenedxConfig
  { openedxClientId     :: !Text
  , openedxClientSecret :: !Text 
  , openedxUrl          :: !BaseUrl
  }
  deriving (Eq, Show, Generic)

options :: Options
options = defaultOptions
  { fieldLabelModifier = camelToSnakeCase "openedx"
  }

instance ToJSON OpenedxConfig where
  toJSON = genericToJSON options

instance FromJSON OpenedxConfig where
  parseJSON = genericParseJSON options

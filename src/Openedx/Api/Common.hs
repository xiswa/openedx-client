{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
module Openedx.Api.Common (
    OauthRequest(..)
  , OauthResponse(..)
  , mkManager
  , openedxOptions
  , openedxFormOptions
  , getAccessToken
  , oauthToken
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Text
import qualified Web.FormUrlEncoded as W
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics
import Xiswa.Utils
import Openedx.Config

openedxFormOptions :: W.FormOptions
openedxFormOptions = W.FormOptions
  { W.fieldLabelModifier = camelToSnakeCase "oReq"
  }

openedxOptions :: Options
openedxOptions = defaultOptions
  { omitNothingFields = True
  }

-- | OpenEdX Oauth endpoint
newtype Oauth route = Oauth
  { _getAccessToken :: route
      :- "oauth2"
      :> "access_token"
      :> ReqBody '[FormUrlEncoded] OauthRequest
      :> Post '[JSON] OauthResponse
  }
  deriving (Generic)

mkManager
  :: forall env m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     )
  => m Manager
mkManager = do
  OpenedxConfig{..} <- grab
  liftIO $ case baseUrlScheme openedxUrl of
    Http  -> newManager defaultManagerSettings
    Https -> newManager tlsManagerSettings

oauthRoute
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> Oauth (AsClientT m)
oauthRoute errorConv = genericClientHoist $ \c -> do
  OpenedxConfig{..} <- grab
  manager <- mkManager
  let env = mkClientEnv manager openedxUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

getAccessToken
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> m OauthResponse
getAccessToken errorConv = do
  OpenedxConfig{..} <- grab
  let oauthRequest = OauthRequest "client_credentials" openedxClientId openedxClientSecret
  _getAccessToken (oauthRoute errorConv) oauthRequest

-- e.g. "Bearer <access_token>"
oauthToken :: OauthResponse -> Text
oauthToken resp = oRespTokenType resp <> " " <> oRespAccessToken resp


data OauthRequest = OauthRequest
  { oReqGrantType       :: !Text
  , oReqClientId        :: !Text
  , oReqClientSecret    :: !Text
  }
  deriving (Eq, Show, Generic)

instance W.ToForm OauthRequest where
  toForm = W.genericToForm openedxFormOptions

instance W.FromForm OauthRequest where
  fromForm = W.genericFromForm openedxFormOptions


data OauthResponse = OauthResponse
  { oRespAccessToken    :: !Text
  , oRespExpiresIn      :: !Int
  , oRespTokenType      :: !Text
  , oRespScope          :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON OauthResponse where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "oResp"
      }

instance FromJSON OauthResponse where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "oResp"
      }

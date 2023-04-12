{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Openedx.Client (
    HasOpenedxConfig(..)
  , HasErrorConv(..)
  , OpenedxConfig(..)
  , WithOpenedx

  -- Client functions
  , getAccessToken
  , bulkEnroll
  , createUser
  , getUser
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson.TH
import Data.Bifunctor
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics

import Openedx.Types
import Openedx.Internal.Utils

class HasOpenedxConfig env where
  getConfig :: env -> OpenedxConfig

class HasErrorConv err env where
  getErrorConv :: env -> ClientError -> err

data OpenedxConfig = OpenedxConfig
  { openedxClientId     :: !Text
  , openedxClientSecret :: !Text 
  , openedxUrl          :: !BaseUrl
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier = tail . camelToSnake . removePrefix "openedx"
  } ''OpenedxConfig)

instance HasOpenedxConfig OpenedxConfig where
  getConfig = id

type WithOpenedx env err m =
  ( MonadReader env m
  , HasOpenedxConfig env
  , HasErrorConv err env
  , MonadIO m
  , MonadError err m
  )


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
  :: forall env err m. (WithOpenedx env err m)
  => m Manager
mkManager = do
  OpenedxConfig{..} <- asks getConfig
  liftIO $ case baseUrlScheme openedxUrl of
    Http  -> newManager defaultManagerSettings
    Https -> newManager tlsManagerSettings

oauthRoute
  :: forall env err m. (WithOpenedx env err m)
  => Oauth (AsClientT m)
oauthRoute = genericClientHoist $ \c -> do
  OpenedxConfig{..} <- asks getConfig
  errorConv <- asks getErrorConv
  manager <- mkManager
  let env = mkClientEnv manager openedxUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

getAccessToken
  :: forall env err m. (WithOpenedx env err m)
  => m OauthResponse
getAccessToken = do
  OpenedxConfig{..} <- asks getConfig
  let oauthRequest = OauthRequest "client_credentials" openedxClientId openedxClientSecret
  _getAccessToken oauthRoute oauthRequest


-- | OpenEdX API endpoints
data Openedx route = Openedx
  {
    -- | Enrollment
    _bulkEnroll :: route
      :- "bulk_enroll"
      :> "v1"
      :> "bulk_enroll"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] BulkEnrollmentRequest
      :> Post '[JSON] BulkEnrollmentResponse

  -- | User management
  , _createUser :: route
      :- "user"
      :> "v2"
      :> "account"
      :> "registration"
      :> Header "Authorization" Text
      :> ReqBody '[FormUrlEncoded] UserCreateRequest
      :> Post '[JSON] UserCreateResponse

  , _getUser :: route
      :- "user"
      :> "v1"
      :> "accounts"
      :> Header "Authorization" Text
      :> Capture "userName" Text
      :> Get '[JSON] User
  }
  deriving (Generic)

openedxRoutes
  :: forall env err m. (WithOpenedx env err m)
  => Openedx (AsClientT m)
openedxRoutes = genericClientHoist $ \c -> do
  OpenedxConfig{..} <- asks getConfig
  errorConv <- asks getErrorConv
  manager <- mkManager
  let apiUrl = openedxUrl { baseUrlPath = "api" }
      env = mkClientEnv manager apiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

-- e.g. "Bearer <access_token>"
oauthToken :: OauthResponse -> Text
oauthToken resp = oRespTokenType resp <> " " <> oRespAccessToken resp

bulkEnroll
  :: forall env err m. (WithOpenedx env err m)
  => BulkEnrollmentRequest
  -> m BulkEnrollmentResponse
bulkEnroll bulkEnrollmentRequest = do
  oauthResp <- getAccessToken
  _bulkEnroll openedxRoutes (Just $ oauthToken oauthResp) bulkEnrollmentRequest

createUser
  :: forall env err m. (WithOpenedx env err m)
  => UserCreateRequest
  -> m UserCreateResponse
createUser userCreateRequest = do
  oauthResp <- getAccessToken
  _createUser openedxRoutes (Just $ oauthToken oauthResp) userCreateRequest
  
getUser
  :: forall env err m. (WithOpenedx env err m)
  => Text
  -> m User
getUser userName = do
  oauthResp <- getAccessToken
  _getUser openedxRoutes (Just $ oauthToken oauthResp) userName

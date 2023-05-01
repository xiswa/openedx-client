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
module Openedx.Api.User (
    UserCreateRequest(..)
  , UserCreateResponse(..)
  , User(..)
  , createUser
  , getUser
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Text
import qualified Web.FormUrlEncoded as W
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics
import Xiswa.Utils
import Openedx.Config
import Openedx.Api.Common

data UserR route = UserR
  {
    _createUser :: route
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

userRoutes
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> UserR (AsClientT m)
userRoutes errorConv = genericClientHoist $ \c -> do
  OpenedxConfig{..} <- grab
  manager <- mkManager
  let apiUrl = openedxUrl { baseUrlPath = "api" }
      env = mkClientEnv manager apiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

createUser
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> UserCreateRequest
  -> m UserCreateResponse
createUser errorConv userCreateRequest = do
  oauthResp <- getAccessToken errorConv
  _createUser (userRoutes errorConv) (Just $ oauthToken oauthResp) userCreateRequest
  
getUser
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , MonadIO m
     , MonadError err m
     )
  => (ClientError -> err)
  -> Text
  -> m User
getUser errorConv userName = do
  oauthResp <- getAccessToken errorConv
  _getUser (userRoutes errorConv) (Just $ oauthToken oauthResp) userName


data UserCreateRequest = UserCreateRequest
  { ucReqUsername    :: !Text
  , ucReqEmail       :: !Text
  , ucReqPassword    :: !Text
  , ucReqName        :: !Text
  , ucTermsOfService :: !Bool
  }
  deriving (Eq, Show, Generic)

instance W.ToForm UserCreateRequest where
  toForm = W.genericToForm openedxFormOptions

instance W.FromForm UserCreateRequest where
  fromForm = W.genericFromForm openedxFormOptions


data UserCreateResponse = UserCreateResponse
  { ucRespSuccess     :: !Bool
  , ucRespRedirectUrl :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserCreateResponse where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "ucResp"
      }

instance FromJSON UserCreateResponse where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "ucResp"
      }


data User = User
  { userId       :: !Int
  , userEmail    :: !Text
  , userName     :: !Text
  , userUsername :: !Text
  , userIsActive :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "user"
      }

instance FromJSON User where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "user"
      }

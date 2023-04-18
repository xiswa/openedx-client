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
module Openedx.Api.Enrollment (
    Action(..) 
  , EnrollmentStatus(..)
  , ActionResult(..)
  , BulkEnrollmentRequest(..)
  , BulkEnrollmentResponse(..)
  , Course(..)
  , bulkEnroll
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Text (Text)
import Data.Map (Map)
import Data.Char (toLower)
import Servant.API
import Servant.Client
import Servant.Client.Generic
import GHC.Generics
import Xiswa.Utils
import Openedx.Config
import Openedx.Api.Common

newtype EnrollR route = EnrollR
  {
    _bulkEnroll :: route
      :- "bulk_enroll"
      :> "v1"
      :> "bulk_enroll"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] BulkEnrollmentRequest
      :> Post '[JSON] BulkEnrollmentResponse
  }
  deriving (Generic)

enrollmentRoutes
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => EnrollR (AsClientT m)
enrollmentRoutes = genericClientHoist $ \c -> do
  OpenedxConfig{..} <- grab
  errorConv <- grab
  manager <- mkManager
  let apiUrl = openedxUrl { baseUrlPath = "api" }
      env = mkClientEnv manager apiUrl
  resp <- liftIO (runClientM c env)
  liftEither $ first errorConv resp

bulkEnroll
  :: forall env err m.
     ( MonadReader env m
     , Has OpenedxConfig env
     , Has (ClientError -> err) env
     , MonadIO m
     , MonadError err m
     )
  => BulkEnrollmentRequest
  -> m BulkEnrollmentResponse
bulkEnroll bulkEnrollmentRequest = do
  oauthResp <- getAccessToken
  _bulkEnroll enrollmentRoutes (Just $ oauthToken oauthResp) bulkEnrollmentRequest


data Action = Enroll | Unenroll
  deriving (Eq, Show, Generic)

instance ToJSON Action where
  toJSON = genericToJSON $
    openedxOptions
      { constructorTagModifier = map toLower
      }

instance FromJSON Action where
  parseJSON = genericParseJSON $
    openedxOptions
      { constructorTagModifier = map toLower
      }


data EnrollmentStatus = EnrollmentStatus
  { esUser       :: !Bool
  , esEnrollment :: !Bool
  , esAllowed    :: !Bool
  , esAutoEnroll :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON EnrollmentStatus where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "es"
      }

instance FromJSON EnrollmentStatus where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "es"
      }


data ActionResult = ActionResult
  { arIdentifier :: !Text
  , arBefore     :: !EnrollmentStatus
  , arAfter      :: !EnrollmentStatus
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActionResult where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "ar"
      }

instance FromJSON ActionResult where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "ar"
      }


data BulkEnrollmentRequest = BulkEnrollmentRequest
  { beReqAutoEnroll     :: !Bool
  , beReqEmailStudents  :: !Bool
  , beReqAction         :: !Action
  , beReqCourses        :: Text
  , beReqCohorts        :: Maybe Text
  , beReqIdentifiers    :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON BulkEnrollmentRequest where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "beReq"
      }

instance FromJSON BulkEnrollmentRequest where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "beReq"
      }


data Course = Course
  { courseResults       :: [ActionResult]
  , courseAutoEnroll    :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Course where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "course"
      }

instance FromJSON Course where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "course"
      }


data BulkEnrollmentResponse = BulkEnrollmentResponse
  { beRespAutoEnroll    :: !Bool
  , beRespEmailStudents :: !Bool
  , beRespAction        :: !Action
  , beRespCourses       :: Map Text Course
  }
  deriving (Eq, Show, Generic)

instance ToJSON BulkEnrollmentResponse where
  toJSON = genericToJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "beResp"
      }

instance FromJSON BulkEnrollmentResponse where
  parseJSON = genericParseJSON $
    openedxOptions
      { fieldLabelModifier = camelToSnakeCase "beResp"
      }

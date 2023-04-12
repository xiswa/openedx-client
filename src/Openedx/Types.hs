{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
module Openedx.Types (
  -- | Auth
    OauthRequest(..)
  , OauthResponse(..)

  -- | User management
  , UserCreateRequest(..)
  , UserCreateResponse(..)
  , User(..)

  -- | Enrollment
  , Action(..)
  , EnrollmentStatus(..)
  , Course(..)
  , ActionResult(..)
  , BulkEnrollmentRequest(..)
  , BulkEnrollmentResponse(..)
  ) where

import GHC.Generics
import Data.Char (toLower)
import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.TH
import qualified Web.FormUrlEncoded as W

import Xiswa.Utils

data OauthRequest = OauthRequest
  { oReqGrantType       :: !Text
  , oReqClientId        :: !Text
  , oReqClientSecret    :: !Text
  }
  deriving (Eq, Show, Generic)

formOptions :: W.FormOptions
formOptions = W.FormOptions
  { W.fieldLabelModifier = camelToSnakeWithPref "oReq"
  }

instance W.ToForm OauthRequest where
  toForm = W.genericToForm formOptions

instance W.FromForm OauthRequest where
  fromForm = W.genericFromForm formOptions

data OauthResponse = OauthResponse
  { oRespAccessToken    :: !Text
  , oRespExpiresIn      :: !Int
  , oRespTokenType      :: !Text
  , oRespScope          :: !Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "oResp"
  } ''OauthResponse)


data UserCreateRequest = UserCreateRequest
  { ucReqUsername       :: !Text
  , ucReqEmail          :: !Text
  , ucReqPassword       :: !Text
  , ucReqName           :: !Text
  , ucTermsOfService    :: !Bool
  }
  deriving (Eq, Show, Generic)

instance W.ToForm UserCreateRequest where
  toForm = W.genericToForm formOptions

instance W.FromForm UserCreateRequest where
  fromForm = W.genericFromForm formOptions

data UserCreateResponse = UserCreateResponse
  { ucRespSuccess       :: !Bool
  , ucRespRedirectUrl   :: !Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "ucResp"
  } ''UserCreateResponse)

data User = User
  { userId              :: !Int
  , userEmail           :: !Text
  , userName            :: !Text
  , userUsername        :: !Text
  , userIsActive        :: !Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "user"
  } ''User)

data Action = Enroll | Unenroll
  deriving (Eq, Show)

$(deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Action)

data EnrollmentStatus = EnrollmentStatus
  { esUser              :: !Bool
  , esEnrollment        :: !Bool
  , esAllowed           :: !Bool
  , esAutoEnroll        :: !Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "es"
  } ''EnrollmentStatus)

data ActionResult = ActionResult
  { arIdentifier        :: !Text
  , arBefore            :: !EnrollmentStatus
  , arAfter             :: !EnrollmentStatus
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "ar"
  } ''ActionResult)

data BulkEnrollmentRequest = BulkEnrollmentRequest
  { beReqAutoEnroll     :: !Bool
  , beReqEmailStudents  :: !Bool
  , beReqAction         :: !Action
  , beReqCourses        :: Text
  , beReqCohorts        :: Maybe Text
  , beReqIdentifiers    :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "beReq"
  , omitNothingFields   = True
  } ''BulkEnrollmentRequest)

data Course = Course
  { courseResults       :: [ActionResult]
  , courseAutoEnroll    :: !Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "course"
  } ''Course)

data BulkEnrollmentResponse = BulkEnrollmentResponse
  { beRespAutoEnroll    :: !Bool
  , beRespEmailStudents :: !Bool
  , beRespAction        :: !Action
  , beRespCourses       :: Map Text Course
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = camelToSnakeWithPref "beResp"
  } ''BulkEnrollmentResponse)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Openedx.Types where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.TH
import Openedx.Internal.Utils (removePrefix, camelToSnake)

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
    fieldLabelModifier  = tail . camelToSnake . removePrefix "es"
  } ''EnrollmentStatus)

data ActionResult = ActionResult
  { arIdentifier        :: !Text
  , arBefore            :: !EnrollmentStatus
  , arAfter             :: !EnrollmentStatus
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = tail . camelToSnake . removePrefix "ar"
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
    fieldLabelModifier  = tail . camelToSnake . removePrefix "beReq"
  , omitNothingFields   = True
  } ''BulkEnrollmentRequest)

data Course = Course
  { courseResults       :: [ActionResult]
  , courseAutoEnroll    :: !Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = tail . camelToSnake . removePrefix "course"
  } ''Course)

data BulkEnrollmentResponse = BulkEnrollmentResponse
  { beRespAutoEnroll    :: !Bool
  , beRespEmailStudents :: !Bool
  , beRespAction        :: !Action
  , beRespCourses       :: Map Text Course
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier  = tail . camelToSnake . removePrefix "beResp"
  } ''BulkEnrollmentResponse)

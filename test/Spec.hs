{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
module Main (main) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe (fromJust)
import Data.Aeson
import Data.Text (Text)
import Servant.Client
import Test.Hspec
import Network.HTTP.Types
import qualified Data.Map as M

import Openedx

newtype TestEnv = TestEnv 
  { unTestEnv :: (OpenedxConfig, ClientError -> ClientError)
  }

instance HasOpenedxConfig TestEnv where
  getConfig = fst . unTestEnv

instance HasErrorConv ClientError TestEnv where
  getErrorConv = snd . unTestEnv

newtype Test a = Test
  { unTest :: ReaderT TestEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)

instance MonadError ClientError Test where
  throwError :: ClientError -> Test a
  throwError = liftIO . throwIO
  {-# INLINE throwError #-}

  catchError :: Test a -> (ClientError -> Test a) -> Test a
  catchError action handler = Test $ ReaderT $ \env -> do
    let ioAction = runTest env action
    ioAction `catch` \e -> runTest env $ handler e
  {-# INLINE catchError #-}

runTest :: TestEnv -> Test a -> IO a
runTest env = flip runReaderT env . unTest

spec :: TestEnv -> Spec
spec env = do
  describe "getAccessToken" $ do
    it "responds with a valid access token" $ do
      resp <- runTest env getAccessToken
      oRespTokenType resp `shouldBe` "Bearer"

  -- NOTE: needs to have "wahyu" already created and "tri" not created
  describe "getUser" $ do
    let err404 :: Selector ClientError
        err404 (FailureResponse _ resp) = responseStatusCode resp == status404
        err404 _                        = False
    it "lookup user correctly" $ do
      wahyu <- runTest env (getUser "wahyu")
      wahyu `shouldBe` User 5 "wahyu@gmail.com" "" "wahyu" True

      runTest env (getUser "tri") `shouldThrow` err404

  -- NOTE: needs to have the course created first
  describe "bulkEnroll" $ do
    let enrollRequest = BulkEnrollmentRequest
          { beReqAutoEnroll = True
          , beReqEmailStudents = False
          , beReqAction = Enroll
          , beReqCourses = "course-v1:AsalX+CS101+2023_T2"
          , beReqCohorts = Nothing
          , beReqIdentifiers = "wahyu@gmail.com"
          }

        unenrollRequest = enrollRequest { beReqAction = Unenroll }

        enrollmentStatus :: Text -> BulkEnrollmentResponse -> EnrollmentStatus
        enrollmentStatus courseName (BulkEnrollmentResponse _ _ _ respCourses) =
          let [result] = courseResults $ respCourses M.! courseName 
           in arAfter result
          
    it "enrolls correctly" $ do
      resp <- runTest env (bulkEnroll enrollRequest)
      esEnrollment (enrollmentStatus "course-v1:AsalX+CS101+2023_T2" resp) `shouldBe` True

    it "unenrolls correctly" $ do
      resp <- runTest env (bulkEnroll unenrollRequest)
      esEnrollment (enrollmentStatus "course-v1:AsalX+CS101+2023_T2" resp) `shouldBe` False
    
  describe "JSON instances" $ do
    it "correctly encodes and decodes Actions" $ do
      encode Enroll         `shouldBe` "\"enroll\""
      encode Unenroll       `shouldBe` "\"unenroll\""
      decode "\"enroll\""   `shouldBe` Just Enroll
      decode "\"unenroll\"" `shouldBe` Just Unenroll

main :: IO ()
main = do
  config <- fromJust <$> decodeFileStrict' "config.json"
  hspec $ spec $ TestEnv (config, id)

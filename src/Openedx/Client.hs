{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Openedx.Client where

import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client

import Openedx.Types

type API = 
  "bulk_enroll" :> Header "Authorization" Text :> ReqBody '[JSON] BulkEnrollmentRequest :> Post '[JSON] BulkEnrollmentResponse

enrollUser :: Maybe Text -> BulkEnrollmentRequest -> ClientM BulkEnrollmentResponse
enrollUser = client (Proxy @API)

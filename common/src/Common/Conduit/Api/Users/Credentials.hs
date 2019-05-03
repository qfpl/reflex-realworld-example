{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Users.Credentials where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Credentials = Credentials
  { email :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON Credentials
instance ToJSON Credentials

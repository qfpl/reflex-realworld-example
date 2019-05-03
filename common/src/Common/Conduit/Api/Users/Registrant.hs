{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Users.Registrant where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Registrant = Registrant
  { username :: Text
  , email :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON Registrant
instance ToJSON Registrant

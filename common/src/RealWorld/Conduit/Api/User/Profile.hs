{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RealWorld.Conduit.Api.User.Profile where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Profile = Profile
  { id        :: Int
  , username  :: Text
  , bio       :: Text
  , image     :: Maybe Text
  , following :: Bool
  } deriving (Generic, Eq)

deriving instance ToJSON Profile
deriving instance FromJSON Profile

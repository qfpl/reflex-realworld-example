{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module RealWorld.Conduit.Api.Profile where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Get, JSON, Capture)

import           RealWorld.Conduit.Api.Namespace         (Namespace)

type ProfileApi token = Capture "username" Text :> Get '[JSON] (Namespace "profile" Profile)

data Profile = Profile
  { id        :: Int
  , username  :: Text
  , bio       :: Text
  , image     :: Maybe Text
  , following :: Bool
  } deriving (Generic, Eq)

deriving instance ToJSON Profile
deriving instance FromJSON Profile

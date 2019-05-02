{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.User.Update where

import           Data.Aeson   (ToJSON)
import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data UpdateUser = UpdateUser
  { password :: Maybe Text
  , email    :: Maybe Text
  , username :: Maybe Text
  , bio      :: Maybe Text
  , image    :: Maybe Text -- Danger: This means that we can't unset this thing
  } deriving Generic

deriving instance ToJSON UpdateUser
deriving instance FromJSON UpdateUser

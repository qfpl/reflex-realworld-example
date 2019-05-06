{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
module Common.Conduit.Api.Articles.Comment where

import Common.Conduit.Api.Profiles (Profile)
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import GHC.Generics                (Generic)

data Comment = Comment
  { id        :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body      :: Text
  , author    :: Profile
  } deriving (Eq)

deriving instance Generic Comment
deriving instance ToJSON Comment
deriving instance FromJSON Comment

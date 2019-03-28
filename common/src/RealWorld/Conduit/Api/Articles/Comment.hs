{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RealWorld.Conduit.Api.Articles.Comment where

import           Data.Aeson                         (FromJSON (..), ToJSON (..))
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime)
import           GHC.Generics                       (Generic)
import           RealWorld.Conduit.Api.Profile      (Profile)

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

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Articles.CreateComment where

import           Data.Aeson                         (FromJSON (..), ToJSON (..))
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

data CreateComment = CreateComment
  { body :: Text }

deriving instance Generic CreateComment
deriving instance ToJSON CreateComment
deriving instance FromJSON CreateComment

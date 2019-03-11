{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RealWorld.Conduit.Api.Articles.Article where

import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..))
import           Data.Set                            (Set)
import           Data.Text                           (Text)
import           Data.Time                           (UTCTime)
import           GHC.Generics                        (Generic)
import           RealWorld.Conduit.Api.User.Profile  (Profile)

data Article = Article
  { id             :: Int
  , slug           :: Text
  , title          :: Text
  , description    :: Text
  , body           :: Text
  , tagList        :: Set Text
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  , favorited      :: Bool
  , favoritesCount :: Int
  , author         :: Profile
  }

deriving instance Generic Article
deriving instance ToJSON Article
deriving instance FromJSON Article

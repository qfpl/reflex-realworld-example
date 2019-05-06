{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Articles.Articles where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))
import           GHC.Generics                           (Generic)

import           Common.Conduit.Api.Articles.Article (Article)

data Articles = Articles
  { articles      :: [Article]
  , articlesCount :: Int
  }

fromList :: [Article] -> Articles
fromList = Articles <$> id <*> length

deriving instance Generic Articles
deriving instance ToJSON Articles
deriving instance FromJSON Articles

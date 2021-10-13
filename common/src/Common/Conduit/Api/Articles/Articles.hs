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
  , articlesCount :: Integer
  } deriving Show

fromList :: [Article] -> Articles
fromList = Articles <$> id <*> (toInteger . length)

deriving instance Generic Articles
deriving instance ToJSON Articles
deriving instance FromJSON Articles

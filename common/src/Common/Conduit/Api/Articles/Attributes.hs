{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Common.Conduit.Api.Articles.Attributes where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Functor.Identity           (Identity)
import           Data.Set                        (Set)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Common.Conduit.Api.Attribute (Attribute)

data ArticleAttributes f = ArticleAttributes
  { title       :: Attribute f Text
  , description :: Attribute f Text
  , body        :: Attribute f Text
  , tagList     :: Attribute f (Set Text)
  }

type CreateArticle = ArticleAttributes Identity

deriving instance Generic CreateArticle
deriving instance ToJSON CreateArticle
deriving instance FromJSON CreateArticle

type UpdateArticle = ArticleAttributes Maybe

deriving instance Generic UpdateArticle
deriving instance ToJSON UpdateArticle
deriving instance FromJSON UpdateArticle

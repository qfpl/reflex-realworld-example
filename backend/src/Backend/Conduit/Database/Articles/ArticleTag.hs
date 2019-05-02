{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances                    #-}
module Backend.Conduit.Database.Articles.ArticleTag
  ( ArticleTagT(..)
  , ArticleTag
  ) where

import Database.Beam (Beamable, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

import Backend.Conduit.Database.Articles.Article (ArticleT)
import Backend.Conduit.Database.Tags.Tag         (TagT)

data ArticleTagT f = ArticleTag
  { article :: PrimaryKey ArticleT f
  , tag     :: PrimaryKey TagT f
  } deriving (Generic)

type ArticleTag = ArticleTagT Identity

deriving instance Show ArticleTag

deriving instance Eq ArticleTag

instance Beamable ArticleTagT

instance Beamable (PrimaryKey ArticleTagT)

instance Table ArticleTagT where
  data PrimaryKey ArticleTagT f
    = ArticleTagId
        (PrimaryKey ArticleT f)
        (PrimaryKey TagT f)
    deriving Generic
  primaryKey =
    ArticleTagId
      <$> article
      <*> tag

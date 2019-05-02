{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances                    #-}
module Backend.Conduit.Database.Users.Follow
  ( FollowT(..)
  , Follow
  ) where

import GHC.Generics (Generic)

import Backend.Conduit.Database.Users.User (UserT)

import Database.Beam (Beamable, Identity, PrimaryKey, Table (..))

data FollowT f = Follow
  { follower :: PrimaryKey UserT f
  , followee :: PrimaryKey UserT f
  }

deriving instance Generic (FollowT f)
deriving instance Beamable FollowT

type Follow = FollowT Identity

instance Table FollowT where
  data PrimaryKey FollowT f
    = FollowId
        (PrimaryKey UserT f)
        (PrimaryKey UserT f)
  primaryKey = FollowId <$> follower <*> followee

deriving instance Generic (PrimaryKey FollowT f)
deriving instance Beamable (PrimaryKey FollowT)

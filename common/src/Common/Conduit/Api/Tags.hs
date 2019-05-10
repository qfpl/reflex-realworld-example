{-# LANGUAGE DataKinds, TypeOperators #-}
module Common.Conduit.Api.Tags (TagsApi) where

import Data.Text   (Text)
import Servant.API (Get, JSON)

import Common.Conduit.Api.Namespace (Namespace)

type TagsApi token = Get '[JSON] (Namespace "tags" [Text])

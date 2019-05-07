{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}
module Common.Conduit.Api.Errors
  ( ErrorBody(..)
  ) where

import Data.Aeson           (ToJSON, FromJSON)
import Data.Text            (Text)
import GHC.Generics         (Generic)

data ErrorBody errors = ErrorBody
  { message :: Text
  , errors  :: Maybe errors
  } deriving (Generic, Show)

deriving instance ToJSON errors   => ToJSON (ErrorBody errors)
deriving instance FromJSON errors => FromJSON (ErrorBody errors)

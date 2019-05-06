{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.User.Account where

import Data.Aeson          (FromJSON (..), ToJSON (..))
import Data.Text           (Text)
import GHC.Generics        (Generic)

newtype Token = Token { getToken :: Text } deriving (Show, Generic)

data Account = Account
  { email    :: Text
  , token    :: Token
  , username :: Text
  , bio      :: Text
  , image    :: Maybe Text
  } deriving (Show, Generic)

deriving instance ToJSON Account
deriving instance FromJSON Account

instance ToJSON Token where
  toJSON = toJSON . getToken

instance FromJSON Token where
  parseJSON = fmap Token . parseJSON

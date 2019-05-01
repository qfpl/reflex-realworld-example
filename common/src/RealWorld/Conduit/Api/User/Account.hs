{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RealWorld.Conduit.Api.User.Account where

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

deriving instance ToJSON Token
deriving instance FromJSON Token

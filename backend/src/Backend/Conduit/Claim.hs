{-# LANGUAGE DeriveGeneric #-}
module Backend.Conduit.Claim
  ( Claim(..)
  , fromUser
  , deriveToken
  ) where

import           Control.Monad.Except    (ExceptT (ExceptT))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Crypto.JOSE             (Error)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Database.Beam           (primaryKey)
import           GHC.Generics            (Generic)
import           Servant.Auth.Server     (FromJWT, JWTSettings, ToJWT)
import qualified Servant.Auth.Server     as ServantAuth

import Backend.Conduit.Database.Users.User (PrimaryKey (unUserId), User)

newtype Claim =
  Claim { id :: Int }
  deriving (Generic)

instance ToJSON Claim
instance FromJSON Claim
instance ToJWT Claim
instance FromJWT Claim

fromUser :: User -> Claim
fromUser =
  Claim . unUserId . primaryKey

deriveToken :: MonadIO m => JWTSettings -> User -> ExceptT Error m Text
deriveToken settings user =
  fmap (toStrict . decodeUtf8) . ExceptT . liftIO $ ServantAuth.makeJWT (fromUser user) settings Nothing

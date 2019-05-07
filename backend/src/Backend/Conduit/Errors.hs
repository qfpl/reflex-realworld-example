{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module Backend.Conduit.Errors
  ( ErrorBody(..)
  , ConduitErrorsT
  , failedValidation
  , forbidden
  , internalServerError
  , internalServerErrorShow
  , notAuthorized
  , notFound
  , runConduitErrorsT
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson           (ToJSON, encode)
import Data.Text            (Text, pack)
import Servant              (ServantErr (..), err401, err403, err404, err500, errBody, throwError)
import Snap.Core            (MonadSnap)

import Common.Conduit.Api.Errors

type ConduitErrorsT m = ExceptT ServantErr m

runConduitErrorsT :: MonadSnap m => ConduitErrorsT m a -> m a
runConduitErrorsT = (either throwError pure  =<<) . runExceptT

notAuthorized :: ServantErr
notAuthorized =
  err401 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Not Authorized"
        , errors = Nothing
        }

forbidden :: ServantErr
forbidden =
  err403 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Forbidden"
        , errors = Nothing
        }

notFound :: Text -> ServantErr
notFound resourceName =
  err404 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = resourceName <> " not found"
        , errors = Nothing
        }

failedValidation :: ToJSON failures => failures -> ServantErr
failedValidation failures =
  ServantErr
    { errHTTPCode = 422
    , errReasonPhrase = "Unprocessable Entity"
    , errBody = encode (body failures)
    , errHeaders = []
    }
    where
      body :: failures -> ErrorBody failures
      body fs = ErrorBody
        { message = "Failed validation"
        , errors = Just fs
        }

internalServerError :: Text -> ServantErr
internalServerError msg =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg]
        }

internalServerErrorShow :: Show e => Text -> e -> ServantErr
internalServerErrorShow msg e =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg, pack . show $ e]
        }

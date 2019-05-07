module Common.Conduit.Api.Validation where

import Data.Map  (Map)
import Data.Text (Text)

type ValidationErrors = Map Text [Text]

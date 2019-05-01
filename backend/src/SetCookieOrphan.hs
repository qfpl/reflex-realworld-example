{-# OPTIONS_GHC -fno-warn-orphans #-}
module SetCookieOrphan where
import qualified Data.ByteString.Builder  as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import           Web.Cookie               (SetCookie, parseSetCookie, renderSetCookie)
import           Web.HttpApiData          (FromHttpApiData (..), ToHttpApiData (..))

instance ToHttpApiData SetCookie where
  toUrlPiece = decodeUtf8With lenientDecode . toHeader
  toHeader = LBS.toStrict . BS.toLazyByteString . renderSetCookie

-- | /Note:/ this instance works correctly for alphanumeric name and value
--
-- >>> parseUrlPiece "SESSID=r2t5uvjq435r4q7ib3vtdjq120" :: Either Text SetCookie
-- Right (SetCookie {setCookieName = "SESSID", setCookieValue = "r2t5uvjq435r4q7ib3vtdjq120", setCookiePath = Nothing, setCookieExpires = Nothing, setCookieMaxAge = Nothing, setCookieDomain = Nothing, setCookieHttpOnly = False, setCookieSecure = False, setCookieSameSite = Nothing})
instance FromHttpApiData SetCookie where
  parseUrlPiece = parseHeader  . encodeUtf8
  parseHeader   = Right . parseSetCookie

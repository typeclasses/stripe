{- | https://stripe.com/docs/webhooks/signatures#verify-manually -}

module Stripe.Signature
  ( Sig (..), isSigValid, digest, signedPayload, natBytes, parseSig
  ) where

-- base
import qualified Data.List
import qualified Data.Maybe
import qualified Data.String
import           Numeric.Natural (Natural)
import qualified Text.Read

-- base16-bytestring
import qualified Data.ByteString.Base16 as Base16

-- bytestring
import Data.ByteString (ByteString)

-- cryptonite
import Crypto.Hash     (SHA256)
import Crypto.MAC.HMAC as HMAC

-- memory
import qualified Data.ByteArray

-- stripe-concepts
import Stripe.Concepts (WebhookSecretKey (..))

-- text
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

isSigValid :: Sig -> WebhookSecretKey -> ByteString -> Bool
isSigValid x secret body =
    Data.List.any (Data.ByteArray.eq correctDigest) (sigV1 x)
  where
    correctDigest = digest secret (sigTime x) body

digest :: WebhookSecretKey -> Natural -> ByteString -> HMAC SHA256
digest (WebhookSecretKey secret) time body =
    HMAC.hmac secret (signedPayload time body)

signedPayload :: Natural -> ByteString -> ByteString
signedPayload time body =
    mconcat
        [ natBytes time
        , encodeAscii "."
        , body
        ]

{- | Convert a natural number to the ASCII encoding of its decimal
representation. -}

natBytes :: Natural -> ByteString
natBytes = encodeAscii . (show :: Natural -> String)

encodeAscii :: String -> ByteString
encodeAscii = Data.String.fromString

{- | The relevant bits of data extracted from the Stripe signature header. -}

data Sig =
  Sig
    { sigTime :: Natural
    , sigV1 :: [ByteString]
    }

{- | Parse the Stripe signature header, returning 'Nothing' if parsing fails. -}

parseSig :: Text -> Maybe Sig
parseSig txt =
  let
    parts :: [(Text, Text)]
    parts = splitSig txt
  in
    do
      time <- Data.List.lookup (Text.pack "t") parts
                    >>= (readNatural . Text.unpack)

      let
          v1 = Data.Maybe.mapMaybe
              ( \(k, v) ->
                  if k == Text.pack "v1"
                  then decodeHex v
                  else Nothing
              )
              parts

      pure Sig{ sigTime = time, sigV1 = v1 }

splitSig :: Text -> [(Text, Text)]
splitSig =
    Data.Maybe.catMaybes
    . fmap (split2 (Text.pack "="))
    . Text.splitOn (Text.pack ",")

split2 :: Text -> Text -> Maybe (Text, Text)
split2 pat src =
    let
        (x, y) = Text.breakOn pat src
        y' = Text.drop (Text.length pat) y
    in
        if Text.null y then Nothing else Just (x, y')

{- | Parse a number consisting of one or more digits 0 through 9. -}

readNatural :: String -> Maybe Natural
readNatural = Text.Read.readMaybe

{- | Decodes hexidecimal text as a byte string. The result is a 'Just' value iff
the text contains an even number of characters and consists only of the digits
@0@ through @9@ and letters @a@ through @f@. -}

decodeHex :: Text -> Maybe ByteString
decodeHex = either (const Nothing) Just . Base16.decode . Text.encodeUtf8

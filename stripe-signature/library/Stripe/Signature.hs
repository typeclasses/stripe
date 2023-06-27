-- | https://stripe.com/docs/webhooks/signatures#verify-manually
module Stripe.Signature
  ( Sig (..),
    isSigValid,
    digest,
    signedPayload,
    natBytes,
    parseSig,
  )
where

import Crypto.Hash.SHA256 qualified
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified
import Data.Maybe qualified
import Data.String qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Numeric.Natural (Natural)
import Stripe.Concepts (WebhookSecretKey (..))
import Text.Read qualified

isSigValid :: Sig -> WebhookSecretKey -> ByteString -> Bool
isSigValid x secret body =
  Data.List.any ((==) correctDigest) (sigV1 x)
  where
    correctDigest = digest secret (sigTime x) body

digest :: WebhookSecretKey -> Natural -> ByteString -> ByteString
digest (WebhookSecretKey secret) time body =
  Crypto.Hash.SHA256.hmac secret (signedPayload time body)

signedPayload :: Natural -> ByteString -> ByteString
signedPayload time body =
  mconcat
    [ natBytes time,
      encodeAscii ".",
      body
    ]

-- | Convert a natural number to the ASCII encoding of its decimal
-- representation.
natBytes :: Natural -> ByteString
natBytes = encodeAscii . (show :: Natural -> String)

encodeAscii :: String -> ByteString
encodeAscii = Data.String.fromString

-- | The relevant bits of data extracted from the Stripe signature header.
data Sig = Sig
  { sigTime :: Natural,
    sigV1 :: [ByteString]
  }

-- | Parse the Stripe signature header, returning 'Nothing' if parsing fails.
parseSig :: Text -> Maybe Sig
parseSig txt =
  let parts :: [(Text, Text)]
      parts = splitSig txt
   in do
        time <-
          Data.List.lookup (Text.pack "t") parts
            >>= (readNatural . Text.unpack)

        let v1 =
              Data.Maybe.mapMaybe
                ( \(k, v) ->
                    if k == Text.pack "v1"
                      then decodeHex v
                      else Nothing
                )
                parts

        pure Sig {sigTime = time, sigV1 = v1}

splitSig :: Text -> [(Text, Text)]
splitSig =
  Data.Maybe.catMaybes
    . fmap (split2 (Text.pack "="))
    . Text.splitOn (Text.pack ",")

split2 :: Text -> Text -> Maybe (Text, Text)
split2 pat src =
  let (x, y) = Text.breakOn pat src
      y' = Text.drop (Text.length pat) y
   in if Text.null y then Nothing else Just (x, y')

-- | Parse a number consisting of one or more digits 0 through 9.
readNatural :: String -> Maybe Natural
readNatural = Text.Read.readMaybe

-- | Decodes hexadecimal text as a byte string. The result is a 'Just' value iff
-- the text contains an even number of characters and consists only of the digits
-- @0@ through @9@ and letters @a@ through @f@.
decodeHex :: Text -> Maybe ByteString
decodeHex = either (const Nothing) Just . Base16.decode . Text.encodeUtf8

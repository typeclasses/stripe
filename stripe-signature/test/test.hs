import Control.Monad (unless)
import Data.ByteString qualified
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified
import Data.Foldable (for_)
import Data.Text qualified
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Stripe.Concepts (WebhookSecretKey (..))
import Stripe.Signature
import System.Exit (exitFailure)

data Failure = Failure String

main :: IO ()
main =
  unless (null failures) $ do
    for_ failures $ \(Failure x) -> putStrLn ("🔥 " ++ x)
    exitFailure

failures :: [Failure]
failures = parseFailures ++ validationFailures

parseFailures :: [Failure]
parseFailures =
  case parseSig (Data.Text.pack test_string) of
    Nothing -> [Failure "Signature parsing failed"]
    Just sig ->
      case sigTime sig == test_time of
        True -> []
        False -> [Failure $ "sigTime: " ++ show (sigTime sig)]
        ++ case (Data.ByteString.unpack <$> sigV1 sig) == [test_v1Bytes] of
          True -> []
          False -> [Failure $ "sigV1: " ++ show (Data.ByteString.unpack <$> sigV1 sig)]
  where
    test_string :: String
    test_string =
      "t=1492774577,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e\
      \77a0e56ff536d0ce8e108d8bd,v0=6ffbb59b2300aae63f27240606\
      \9a9788598b792a944a07aba816edb039989a39"

    test_time :: Natural
    test_time = 1492774577

    test_v1Bytes :: [Word8]
    test_v1Bytes =
      [ 82,
        87,
        168,
        105,
        231,
        236,
        235,
        237,
        163,
        42,
        255,
        166,
        44,
        220,
        163,
        250,
        81,
        202,
        215,
        231,
        122,
        14,
        86,
        255,
        83,
        109,
        12,
        232,
        225,
        8,
        216,
        189
      ]

validationFailures :: [Failure]
validationFailures =
  case Base16.decode (Data.ByteString.Char8.pack "f89c0a0e96fa6304a7ebd5ecaf13bc1beb859577cd7cafb7bdbc43c3bc2f6afa") of
    Left _ -> [Failure "Digest decoding failed"]
    Right d ->
      case isSigValid Sig {sigTime = 123, sigV1 = [d]} (WebhookSecretKey (Data.ByteString.Char8.pack "secret")) (Data.ByteString.Char8.pack "hello") of
        True -> []
        False -> [Failure "isSigValid rejects a valid signature"]
        ++ case isSigValid Sig {sigTime = 123, sigV1 = [d]} (WebhookSecretKey (Data.ByteString.Char8.pack "secret")) (Data.ByteString.Char8.pack "hello!") of
          False -> []
          True -> [Failure "isSigValid accepts an invalid signature"]
        ++ case isSigValid Sig {sigTime = 123, sigV1 = [Data.ByteString.Char8.pack "whatever", d]} (WebhookSecretKey (Data.ByteString.Char8.pack "secret")) (Data.ByteString.Char8.pack "hello") of
          True -> []
          False -> [Failure "isSigValid should accept if *any* of the sigs are valid"]

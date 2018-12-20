{-# OPTIONS_GHC -Wall #-}

import Stripe.Signature

-- base
import Control.Monad   (unless)
import Data.Word       (Word8)
import Numeric.Natural (Natural)
import System.Exit     (die)

-- bytestring
import qualified Data.ByteString

-- text
import qualified Data.Text

test_string :: String
test_string =
    "t=1492774577,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e\
    \77a0e56ff536d0ce8e108d8bd,v0=6ffbb59b2300aae63f27240606\
    \9a9788598b792a944a07aba816edb039989a39"

test_time :: Natural
test_time = 1492774577

test_v1Bytes :: [Word8]
test_v1Bytes =
    [82,87,168,105,231,236,235,237,163,42,255,166,44,220,163,250
    ,81,202,215,231,122,14,86,255,83,109,12,232,225,8,216,189]

main :: IO ()
main =
    case parseSig (Data.Text.pack test_string) of
        Nothing -> die "Signature parsing failed"
        Just sig ->
          do
            unless (sigTime sig == test_time) $
                die ("sigTime: " ++ show (sigTime sig))
            unless ((Data.ByteString.unpack <$> sigV1 sig) == [test_v1Bytes]) $
                die ("sigV1: " ++ show (Data.ByteString.unpack <$> sigV1 sig))

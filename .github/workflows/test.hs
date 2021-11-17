import Data.Maybe
import System.Environment
import System.Process

main =
  do
    ghc <- readGHC <$> getEnv "ghc"
    callProcess "cabal" $ "build" : "all" : constraints ghc
    callProcess "cabal" $ "test" : "all" : "--enable-tests" : constraints ghc

x .= Just y  = Just ("--constraint=" ++ x ++ "==" ++ y)
x .= Nothing = Nothing

data GHC = GHC_8_2 | GHC_8_4 | GHC_8_6 | GHC_8_8 | GHC_8_10 | GHC_9_0

readGHC s = case s of
    "8.2"  -> GHC_8_2
    "8.4"  -> GHC_8_4
    "8.6"  -> GHC_8_6
    "8.8"  -> GHC_8_8
    "8.10" -> GHC_8_10
    "9.0"  -> GHC_9_0

constraints ghc = catMaybes
    [ "base" .= case ghc of
          GHC_8_2  -> Just "4.10.*"
          GHC_8_4  -> Just "4.11.*"
          GHC_8_6  -> Just "4.12.*"
          GHC_8_8  -> Just "4.13.*"
          GHC_8_10 -> Just "4.14.*"
          GHC_9_0  -> Just "4.15.*"
    , "aeson" .= case ghc of
          GHC_8_2  -> Just "2.0.0.0"
          _        -> Nothing
    , "bytestring" .= case ghc of
          GHC_8_2  -> Just "0.10.*"
          GHC_9_0  -> Just "0.11.*"
          _        -> Nothing
    , "containers" .= case ghc of
          GHC_8_2 -> Just "0.6.0.1"
          _        -> Nothing
    , "cryptonite" .= case ghc of
          GHC_8_2  -> Just "0.25.*"
          GHC_8_10 -> Just "0.28.*"
          GHC_9_0  -> Just "0.29.*"
          _        -> Nothing
    , "lens" .= case ghc of
          GHC_8_2  -> Just "4.17.*"
          GHC_8_8  -> Just "5.0.*"
          GHC_9_0  -> Just "5.1.*"
          _        -> Nothing
    , "memory" .= case ghc of
          GHC_8_2  -> Just "0.14.*"
          GHC_8_10 -> Just "0.15.*"
          GHC_9_0  -> Just "0.16.*"
          _        -> Nothing
    ]

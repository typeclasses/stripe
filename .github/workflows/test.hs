import Data.Maybe
import System.Environment
import System.Process

main =
  do
    ghc <- readGHC <$> getEnv "ghc"
    projectFile <- return $ case ghc of
        GHC_9_2 -> ["--project-file", "cabal.ghc-9-2.project"]
        _ -> []
    targets <- return ["all"]
    callProcess "cabal" $ ["build"] ++ projectFile ++ targets ++ constraints ghc
    callProcess "cabal" $ ["test"] ++ projectFile ++ targets ++ ["--enable-tests"] ++ constraints ghc

x .= Just y  = Just ("--constraint=" ++ x ++ "==" ++ y)
x .= Nothing = Nothing

data GHC = GHC_8_10 | GHC_9_0 | GHC_9_2

readGHC s = case s of
    "8.10" -> GHC_8_10
    "9.0"  -> GHC_9_0
    "9.2"  -> GHC_9_2

constraints ghc = catMaybes
    [ "base" .= case ghc of
          GHC_8_10 -> Just "4.14.*"
          GHC_9_0  -> Just "4.15.*"
          GHC_9_2  -> Just "4.16.*"
    , "bytestring" .= case ghc of
          GHC_8_10 -> Just "0.10.*"
          GHC_9_0  -> Just "0.11.*"
          _        -> Nothing
    , "cryptohash-sha256" .= case ghc of
          GHC_8_10 -> Just "0.11.101.0"
          GHC_9_0  -> Just "0.11.102.1"
          _        -> Nothing
    , "lens" .= case ghc of
          GHC_8_10 -> Just "5.0.*"
          GHC_9_0  -> Just "5.1.*"
          _        -> Nothing
    , "warp" .= case ghc of
          GHC_9_2  -> Just "3.3.18"
          _        -> Nothing
    ]

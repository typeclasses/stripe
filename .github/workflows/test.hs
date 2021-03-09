import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.2.2" -> callProcess "cabal" ["test", "all"
                  , "--constraint=aeson == 1.4.*"
                  , "--constraint=lens == 4.17.*"
                  ]
      "8.4.4" -> callProcess "cabal" ["test", "all"]
      "8.6.3" -> callProcess "cabal" ["test", "all"]
      "8.8.1" -> callProcess "cabal" ["test", "all"]
      "8.10.1" -> callProcess "cabal" ["test", "all"
                  , "--constraint=aeson == 1.5.*"
                  , "--constraint=lens == 5.0.*"
                  ]

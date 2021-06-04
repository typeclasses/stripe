import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let
      constraints =
        case ghc of
          "8.2.2"  -> [ "--constraint=aeson == 1.4.*"
                      , "--constraint=bytestring == 0.10.*"
                      , "--constraint=cryptonite == 0.25.*"
                      , "--constraint=lens == 4.17.*"
                      , "--constraint=memory == 0.14.*"
                      , "--constraint=scotty == 0.11.*"
                      ]
          "8.4.4"  -> []
          "8.6.3"  -> []
          "8.8.1"  -> []
          "8.10.1" -> [ "--constraint=cryptonite == 0.28.*"
                      , "--constraint=memory == 0.15.*"
                      ]
          "9.0.1"  -> [ "--constraint=aeson == 1.5.*"
                      , "--constraint=bytestring == 0.11.*"
                      , "--constraint=cryptonite == 0.29.*"
                      , "--constraint=lens == 5.0.*"
                      , "--constraint=memory == 0.16.*"
                      , "--constraint=scotty == 0.12.*"
                      ]

    callProcess "cabal" $ "build" : "all" : constraints
    callProcess "cabal" $ "test"  : "all" : constraints

{ mkDerivation, aeson, async, base, base-compat-batteries
, blaze-builder, bytestring, case-insensitive, data-default-class
, directory, exceptions, hspec, hspec-discover, hspec-wai
, http-types, lib, lifted-base, lucid, monad-control, mtl, network
, regex-compat, text, transformers, transformers-base
, transformers-compat, wai, wai-extra, warp, weigh
}:
mkDerivation {
  pname = "scotty";
  version = "0.12.1";
  sha256 = "1e2a33ff37765d9e1de7fc7b8f20a75b91b008ada60c1e445df06c7fc614db4a";
  libraryHaskellDepends = [
    aeson base base-compat-batteries blaze-builder bytestring
    case-insensitive data-default-class exceptions http-types
    monad-control mtl network regex-compat text transformers
    transformers-base transformers-compat wai wai-extra warp
  ];
  testHaskellDepends = [
    async base bytestring data-default-class directory hspec hspec-wai
    http-types lifted-base network text wai
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring data-default-class lucid mtl text transformers
    weigh
  ];
  homepage = "https://github.com/scotty-web/scotty";
  description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
  license = lib.licenses.bsd3;
}

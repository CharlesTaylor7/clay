{ mkDerivation, base, hspec, hspec-discover, mtl, stdenv, text, these}:
mkDerivation {
  pname = "clay";
  version = "0.14.0";
  src = ./..;
  libraryHaskellDepends = [ base mtl text these ];
  testHaskellDepends = [ base hspec hspec-discover mtl text these ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}

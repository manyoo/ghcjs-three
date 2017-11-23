{ mkDerivation, AC-Vector, base, data-default, ghcjs-base
, ghcjs-dom, stdenv, transformers
}:
mkDerivation {
  pname = "ghcjs-three";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    AC-Vector base data-default ghcjs-base ghcjs-dom transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "http://github.com/manyoo/ghcjs-three#readme";
  description = "A Three.js wrapper for GHCJS";
  license = stdenv.lib.licenses.bsd3;
}

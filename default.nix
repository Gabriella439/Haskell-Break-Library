{ mkDerivation, base, mtl, stdenv, transformers }:
mkDerivation {
  pname = "break";
  version = "1.0.1";
  src = ./.;
  libraryHaskellDepends = [ base mtl transformers ];
  description = "Break from a loop";
  license = stdenv.lib.licenses.bsd3;
}

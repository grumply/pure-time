{ mkDerivation, base, hashable, pure-json, pure-txt, stdenv, time
}:
mkDerivation {
  pname = "pure-time";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hashable pure-json pure-txt time ];
  homepage = "github.com/grumply/pure-time";
  license = stdenv.lib.licenses.bsd3;
}

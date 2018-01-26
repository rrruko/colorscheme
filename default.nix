{ mkDerivation, attoparsec, base, split, stdenv, text, vector }:
mkDerivation {
  pname = "colorscheme";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base split text vector ];
  license = stdenv.lib.licenses.mit;
}

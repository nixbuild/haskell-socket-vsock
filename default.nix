{ mkDerivation, base, lib, socket }:
mkDerivation {
  pname = "socket-vsock";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base socket ];
  homepage = "https://github.com/nixbuild/haskell-socket-vsock";
  license = lib.licenses.mit;
}

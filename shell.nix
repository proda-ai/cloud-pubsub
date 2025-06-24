{ pkgs ? import <nixpkgs> { } }:
let
  hsPkgs = import ./default.nix;
  scriptDir = toString ./.;
in
hsPkgs.shellFor {
  withHoogle = false;
  exactDeps = true;
  buildInputs = [ pkgs.docker-compose ];
  tools = {
    cabal = "3.12.1.0";
    hpack = "0.37.0";
  };

  shellHook = ''
    source ${scriptDir}/set-env.sh
    echo "refreshing cabal files via hpack.."
    hpack
  '';
}

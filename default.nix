let
  sources = {
    haskellNix =
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/6cbb6390ae49b6d7983d00f3db7d21ba078f2c96.tar.gz";
        sha256 = "0qj82l72qpnzrxc8rbfklk038ybgfb1s56rnmigjnx3s417a37iv";
      });
  };

  haskellNix = import sources.haskellNix { };

  pkgs = import
    haskellNix.sources.nixpkgs-2105
    haskellNix.nixpkgsArgs;

  out = pkgs.haskell-nix.project
    {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "cloud-pubsub";
        src = ./.;
      };
      modules = [{
        packages.cloud-pubsub.doCheck = false;
        # Using the workaround described here
        # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
        packages.cloud-pubsub.components.tests.cloud-pubsub-test.build-tools = [
          out.hspec-discover
        ];
      }];
    };
in
out

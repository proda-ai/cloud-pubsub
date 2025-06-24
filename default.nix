let
  sources = {
    haskellNix =
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/256e8da232ea566d08575f57b339d0a1f1e29f2d.tar.gz";
        sha256 = "0wsbjgrkba6rdyz99jyr4rwgwgck7ayc0ja1ghmmbp6l6jg34p21";
      });
  };

  haskellNix = import sources.haskellNix { };

  pkgs = import
    haskellNix.sources.nixpkgs-2405
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

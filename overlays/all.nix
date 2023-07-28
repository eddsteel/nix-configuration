self: pkgs:
    rec {
      nixpkgs-local  = pkgs.callPackages ../../../src/nixpkgs {};
      local          = pkgs.callPackages ../pkgs {};
      brainzo        = local.brainzo;
      scripts        = local.scripts;
      git-web-link   = local.git-web-link;
      circleci-cli   = local.circleci-cli;
      hub-local      = nixpkgs-local.hub;
      exfalso        = pkgs.quodlibet;
      wavebox        = local.wavebox;
      zoom-us        = local.zoomus;
    }

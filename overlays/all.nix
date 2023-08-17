self: pkgs:
    rec {
      local          = pkgs.callPackages ../pkgs {};
      brainzo        = local.brainzo;
      scripts        = local.scripts;
      git-web-link   = local.git-web-link;
      circleci-cli   = local.circleci-cli;
      exfalso        = pkgs.quodlibet;
      wavebox        = local.wavebox;
      zoom-us        = local.zoomus;
    }

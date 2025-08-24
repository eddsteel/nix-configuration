self: pkgs:
    rec {
      local                 = pkgs.callPackages ../pkgs {};
      brainzo               = local.brainzo;
      circleci-cli          = local.circleci-cli;
      exfalso               = pkgs.quodlibet;
      git-web-link          = local.git-web-link;
      power-profiles-daemon = pkgs.power-profiles-daemon.overrideAttrs (final: previous:  { doCheck = false; });
      scripts               = local.scripts;
      wavebox               = local.wavebox;
      wvlet                 = local.wvlet;
      zoom-us               = local.zoomus;
      trino                 = local.trino;
    }

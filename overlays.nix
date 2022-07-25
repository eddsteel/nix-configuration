[
  (import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  }))

  (self: pkgs:
    rec {
      nixpkgs-local = pkgs.callPackages ../../src/nixpkgs {};
      local         = pkgs.callPackages ./pkgs {};
      emacs         = pkgs.emacsUnstable;
      brainzo       = local.brainzo;
      scripts       = local.scripts;
      git-web-link  = local.git-web-link;
      circleci-cli  = local.circleci-cli;
      hub-local     = nixpkgs-local.hub;
    })

  (self: pkgs:
    pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin rec {
      mac-apps       = pkgs.callPackages ./pkgs/mac {};
      emacs          = self.nixpkgs-local.emacsMacport;
      bitwarden      = self.mac-apps.bitwarden;
      signal-desktop = self.mac-apps.signal;
      firefox        = self.mac-apps.firefox;
    }
  )
]

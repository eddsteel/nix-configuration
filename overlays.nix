[
  (import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  }))

  (self: pkgs:
    rec {
      nixpkgs-local  = pkgs.callPackages ../../src/nixpkgs {};
      local          = pkgs.callPackages ./pkgs {};
      my-emacs       = pkgs.emacsUnstable;
      brainzo        = local.brainzo;
      scripts        = local.scripts;
      git-web-link   = local.git-web-link;
      circleci-cli   = local.circleci-cli;
      hub-local      = nixpkgs-local.hub;
      exfalso        = pkgs.quodlibet;
      wavebox        = local.wavebox;
      zoom-us        = local.zoomus;
    }
  )

  (self: pkgs:
    pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin rec {
      nixpkgs-local  = pkgs.callPackages ../../src/nixpkgs {};
      mac-apps       = pkgs.callPackages ./pkgs/mac {};
      my-emacs       = pkgs.emacs;
      bitwarden      = self.mac-apps.bitwarden;
      signal-desktop = self.mac-apps.signal;
      firefox        = self.mac-apps.firefox;
      intellij-idea  = self.mac-apps.intellij-idea-ce;
      iterm2         = self.mac-apps.iterm2;
      exfalso        = self.mac-apps.exfalso;
      terraform-docs = self.mac-apps.terraform-docs;
    }
  )
]

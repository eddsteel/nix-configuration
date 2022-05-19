[
  (import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  }))

  (self: super: {
    my-emacs = super.emacsUnstable;
  })

  (self: super:
    let
      pkgs = super;
      nixpkgsLocal = ../../src/nixpkgs;
      macApps = pkgs/mac;
    in
      pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        nixpkgs-local    = import nixpkgsLocal {};
        my-emacs         = self.nixpkgs-local.emacsMacport;
        mac-apps         = import macApps {inherit pkgs;};
        bitwarden        = self.mac-apps.bitwarden;
        signal-desktop   = self.mac-apps.signal;
        firefox          = self.mac-apps.firefox;
      } // {
        circleci-cli     = pkgs.callPackage pkgs/circleci.nix {};
      }
  )
]

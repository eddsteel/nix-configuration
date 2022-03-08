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
    in
      pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        my-emacs         = pkgs.emacsMacPort;
        bitwarden        = pkgs.callPackage pkgs/mac/bitwarden.nix { inherit pkgs; };
        firefox          = pkgs.callPackage pkgs/mac/firefox.nix { inherit pkgs; };
        istat-menus      = pkgs.callPackage pkgs/mac/istat-menus.nix { inherit pkgs; };
        iterm2           = pkgs.callPackage pkgs/mac/iterm.nix { inherit pkgs; };
        spectacle        = pkgs.callPackage pkgs/mac/spectacle.nix { inherit pkgs; };
        wavebox          = pkgs.callPackage pkgs/mac/wavebox.nix { inherit pkgs; };
        xbar             = pkgs.callPackage pkgs/mac/xbar.nix { inherit pkgs; };
        signal-desktop   = pkgs.callPackage pkgs/mac/signal.nix { inherit pkgs; };
        intellij-idea-ce = pkgs.callPackage pkgs/mac/intellij.nix { inherit pkgs; };
      }
  )
]

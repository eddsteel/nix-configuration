{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };

    brainzo = pkgs.callPackage ../../src/brainzo { nixpkgs = pkgs; };
    scripts = pkgs.callPackage ../../src/scripts {};
    git-web-link = pkgs.callPackage ../../src/git-web-link {};

    emacs = pkgs.emacsUnstable;
  } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
    emacs            = pkgs.emacsMacport;
    bitwarden        = pkgs.callPackage pkgs/mac/bitwarden.nix { inherit pkgs; };
    firefox          = pkgs.callPackage pkgs/mac/firefox.nix { inherit pkgs; };
    istat-menus      = pkgs.callPackage pkgs/mac/istat-menus.nix { inherit pkgs; };
    iterm2           = pkgs.callPackage pkgs/mac/iterm.nix { inherit pkgs; };
    spectacle        = pkgs.callPackage pkgs/mac/spectacle.nix { inherit pkgs; };
    wavebox          = pkgs.callPackage pkgs/mac/wavebox.nix { inherit pkgs; };
    xbar             = pkgs.callPackage pkgs/mac/xbar.nix { inherit pkgs; };
    signal           = pkgs.callPackage pkgs/mac/signal.nix { inherit pkgs; };
    intellij-idea-ce = pkgs.callPackage pkgs/mac/intellij.nix { inherit pkgs; };
  };
}

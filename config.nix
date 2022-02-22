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
  };

  prefs = {
    emacs = pkgs: if pkgs.stdenv.isDarwin then pkgs.emacsMacport else pkgs.emacsUnstable;
  };
}

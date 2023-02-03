{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };
  permittedInsecurePackages = [
    "python-2.7.18.6"
  ];
}

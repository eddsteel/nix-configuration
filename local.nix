{pkgs, lib, config, ...}:
let
  newPkgs = rec {
    scripts = pkgs.callPackage ../../src/scripts {};
  };
in {
  home.packages = [ newPkgs.scripts ];
}

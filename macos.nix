{pkgs, ...}:
{
  # TODO use local.nix more generically.
  home.packages = pkgs.callPackage ../../src/scripts {};
}

{ pkgs ? import <nixpkgs> {}}:
{
  brainzo      = pkgs.callPackage ../../../src/brainzo/default.nix {};
  scripts      = pkgs.callPackage ../../../src/scripts/default.nix {};
  git-web-link = pkgs.callPackage ../../../src/git-web-link/default.nix {};
  circleci-cli = pkgs.callPackage ./circleci.nix {};
}

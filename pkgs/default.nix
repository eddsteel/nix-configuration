{ pkgs ? import <nixpkgs> {}}:
{
  brainzo      = pkgs.callPackage ../../brainzo/default.nix {};
  scripts      = pkgs.callPackage ../../scripts/default.nix {};
  git-web-link = pkgs.callPackage ../../git-web-link/default.nix {};
  circleci-cli = pkgs.callPackage ./circleci.nix {};
  wavebox      = pkgs.callPackage ./wavebox.nix {};
}

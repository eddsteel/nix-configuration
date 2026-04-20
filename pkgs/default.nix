{ pkgs ? import <nixpkgs> {} }:
let
  sources = import ../npins;
in {
  brainzo      = import sources.brainzo {};
  git-web-link = import sources.git-web-link {};
  scripts      = import sources.scripts {};
  circleci-cli = pkgs.callPackage ./circleci.nix {};
  wavebox      = pkgs.callPackage ./wavebox.nix {};
  zoomus       = pkgs.callPackage ./zoomus.nix {};
  wvlet        = pkgs.callPackage ./wvlet.nix {};
  trino        = pkgs.callPackage ./trino.nix {};
}

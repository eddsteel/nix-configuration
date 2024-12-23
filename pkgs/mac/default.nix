{ pkgs ? import <nixpkgs> {} }:
with pkgs; {
  bitwarden        = callPackage ./bitwarden.nix {};
  caffeine         = callPackage ./caffeine.nix {};
  firefox          = callPackage ./firefox.nix {};
  intellij-idea-ce = callPackage ./intellij.nix {};
  istat-menus      = callPackage ./istat-menus.nix {};
  signal           = callPackage ./signal.nix {};
  soundsource      = callPackage ./soundsource.nix {};
  xbar             = callPackage ./xbar.nix {};
  exfalso          = callPackage ./exfalso.nix {};
  terraform-docs   = callPackage ./terraform-docs.nix {};
  aws-vpn          = callPackage ./aws-vpn {};
  podman-desktop   = callPackage ./podman-desktop.nix {};
  vfkit            = callPackage ./vfkit.nix {};
}

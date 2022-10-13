{ pkgs ? import <nixpkgs> {}}:
with pkgs; {
  bitwarden        = callPackage ./bitwarden.nix {};
  caffeine         = callPackage ./caffeine.nix {};
  firefox          = callPackage ./firefox.nix {};
  intellij-idea-ce = callPackage ./intellij.nix {};
  istat-menus      = callPackage ./istat-menus.nix {};
  iterm2           = callPackage ./iterm.nix {};
  signal           = callPackage ./signal.nix {};
  soundsource      = callPackage ./soundsource.nix {};
  rectangle        = callPackage ./rectangle.nix {};
  wavebox          = callPackage ./wavebox.nix {};
  xbar             = callPackage ./xbar.nix {};
  exfalso          = callPackage ./exfalso.nix {};
  terraform-docs   = callPackage ./terraform-docs.nix {};
}

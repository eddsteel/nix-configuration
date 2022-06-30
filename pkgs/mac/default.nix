{ pkgs }:
{
  one-password     = pkgs.callPackage ./1password.nix {};
  bitwarden        = pkgs.callPackage ./bitwarden.nix {};
  firefox          = pkgs.callPackage ./firefox.nix {};
  intellij-idea-ce = pkgs.callPackage ./intellij.nix {};
  istat-menus      = pkgs.callPackage ./istat-menus.nix {};
  iterm            = pkgs.callPackage ./iterm.nix {};
  signal           = pkgs.callPackage ./signal.nix {};
  soundsource      = pkgs.callPackage ./soundsource.nix {};
  spectacle        = pkgs.callPackage ./spectacle.nix {};
  wavebox          = pkgs.callPackage ./wavebox.nix {};
  xbar             = pkgs.callPackage ./xbar.nix {};
}

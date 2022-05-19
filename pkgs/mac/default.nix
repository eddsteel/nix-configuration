{ pkgs }:
{
  one-password = pkgs.callPackage ./1password.nix { inherit pkgs; };
  bitwarden = pkgs.callPackage ./bitwarden.nix { inherit pkgs; };
  firefox = pkgs.callPackage ./firefox.nix { inherit pkgs; };
  intellij-idea-ce = pkgs.callPackage ./intellij.nix { inherit pkgs; };
  istat-menus = pkgs.callPackage ./istat-menus.nix { inherit pkgs; };
  iterm = pkgs.callPackage ./iterm.nix { inherit pkgs; };
  signal = pkgs.callPackage ./signal.nix { inherit pkgs; };
  soundsource = pkgs.callPackage ./soundsource.nix { inherit pkgs; };
  spectacle = pkgs.callPackage ./spectacle.nix { inherit pkgs; };
  wavebox = pkgs.callPackage ./wavebox.nix { inherit pkgs; };
  xbar = pkgs.callPackage ./xbar.nix { inherit pkgs; };
}

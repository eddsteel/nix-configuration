{pkgs, config, ...} :
{
  user = "edd";
  email = "edd@eddsteel.com";
  homeDir = "/home/edd";
  src.repos = [
    {"name" = "ledger"; "remote" = "git@eddsteel.com:diane.git"; "stow" = true;}
    {"name" = "df-emacs"; "stow" = true;}
    {"name" = "git-web-link";}
    {"name" = "scripts";}
    {"name" = "nixpkgs"; "remote" = "gh:NixOS/nixpkgs";}
    {"name" = "brainzo";}
  ];
  homePkgs = [pkgs.wmctrl];
  gnome = true;
}

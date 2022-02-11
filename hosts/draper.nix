{pkgs, config, ...} :
{
  name = "draper";
  user = "edd";
  email = "edd@eddsteel.com";
  gpg = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
  homeDir = "/home/edd";
  src.repos = [
    {"name" = "ledger"; "remote" = "git@eddsteel.com:diane.git"; "stow" = true;}
    {"name" = "df-emacs"; "stow" = true; "pfx" = ".config";}
    {"name" = "git-web-link";}
    {"name" = "scripts";}
    {"name" = "nixpkgs"; "remote" = "gh:NixOS/nixpkgs";}
    {"name" = "brainzo";}
    {"name" = "bookbot";}
  ];
  homePkgs = with pkgs; [wmctrl vlc zoom-us psmisc ledger discord];
  gnome = true;
  linux = true;
  macos = false;
  localPkgs = [
    (pkgs.callPackage ../../../src/brainzo {})
    (pkgs.callPackage ../../../src/scripts {})
  ]
}

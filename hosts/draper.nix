{pkgs, config, ...} :
{
  name = "draper";
  email = "edd@eddsteel.com";
  gpg = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
  src.repos = [
    {"name" = "ledger"; "remote" = "git@eddsteel.com:diane.git"; "stow" = true;}
    {"name" = "git-web-link";}
    {"name" = "scripts";}
    {"name" = "nixpkgs"; "remote" = "gh:NixOS/nixpkgs";}
    {"name" = "brainzo";}
    {"name" = "bookbot";}
    {"name" = "tell-consul";}
  ];
  packages = with pkgs; [wmctrl vlc zoom-us psmisc ledger discord];
  shellAliases = {};
  gnome = true;
  linux = true;
  macos = false;
  go = false;
}

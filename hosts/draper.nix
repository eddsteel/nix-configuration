{pkgs, config, ...} :
{
  user = "edd";
  email = "edd@eddsteel.com";
  homeDir = "/home/edd";
  face.url = "https://cdn002.tintin.com/public/tintin/img/characters/le-capitaine-haddock/captain-haddock.png";
  face.sha = "0672magv1193shrmdyjfv1mjmgbaqrhlnsbi9jqk3y5wjx69gfb2";
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

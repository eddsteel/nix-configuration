{ config, pkgs, lib, ... }:
let
  username = "edd";
  homedir = "/home/${username}";
  email = "${username}@eddsteel.com";
  emacs = pkgs.emacs29-pgtk;
in {
  imports = [
    ../../modules/home/linux/gnome.nix
    ../../modules/home/linux
    ../../modules/home
  ];

  home.stateVersion = "21.05";
  programs.home-manager = {
    enable = true;
    path = "${homedir}/src/home-manager";
  };

  home.packages = with pkgs; [
    wmctrl vlc zoom-us psmisc ledger discord nixpkgs-local.wavebox handbrake
  ];

  local = {
    inherit homedir username;
    hostname = "draper";
    ssh = true;
  };

  git = {
    enable = true;
    inherit emacs email;
    key = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
  };

  shell = {
    enable = true;
    inherit emacs;
  };

  workstation = {
    enable = true;
    github-name = "eddsteel";
    mr-repos = [
      {"name" = "ledger"; "remote" = "git@eddsteel.com:diane.git"; "stow" = true;}
      {"name" = "git-web-link";}
      {"name" = "scripts";}
      {"name" = "brainzo";}
      {"name" = "bookbot";}
      {"name" = "tell-consul";}
    ];
  };

  emacs = {
    enable = true;
    package = emacs;
  };

  firefox = {
    enable = true;
    sync-user = email;
  };
}

{ config, pkgs, lib, ... }:
let
  email = "edd@eddsteel.com"
in {
  imports = [ ../../modules/home/linux/gnome.nix ../../modules/home/linux ../../modules/home ];

  home.stateVersion = "21.05";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    wmctrl vlc zoom-us psmisc ledger discord nixpkgs-local.wavebox handbrake
  ];

  local = {
    username = "edd";
    homedir = "/home/edd";
    hostname = "draper";
    ssh = true;
  };

  git = {
    inherit email;
    enable = true;
    key = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
  };

  shell = {
    enable = true;
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
  };

  firefox = {
    enable = true;
    sync-user = email;
  }
}

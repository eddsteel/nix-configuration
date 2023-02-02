{ config, pkgs, lib, ... }:
{
  imports = [
    ../../home/emacs.nix
    ../../home/firefox.nix
    ../../home/git.nix
    ../../home/gnome.nix
    ../../home/linux.nix
    ../../home/local.nix
    ../../home/shell.nix
    ../../home/ssh.nix
    ../../home/workstation.nix
  ];

  home.stateVersion = "21.05";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    wmctrl vlc zoom-us psmisc ledger discord nixpkgs-local.wavebox handbrake
  ];

  local = {
    username = "edd";
    homedir = "/home/edd";
    hostname = "draper";
  };

  programs.git = {
    userEmail = "edd@eddsteel.com";
    signing.key = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
  };

  shell = {
    enable = true;
    extraAliases = {};
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
}

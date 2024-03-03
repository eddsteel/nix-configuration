{ config, pkgs, lib, ... }:
let
  username = "edd";
  homedir = "/home/${username}";
  emacs = pkgs.emacs29-pgtk;
  secrets = builtins.fromTOML (builtins.readFile ./secrets.toml);
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
    discord handbrake imagemagick ledger wavebox psmisc vlc wmctrl zoom-us
    tree-sitter
  ];

  local = {
    inherit homedir username;
    hostname = "draper";
    ssh = true;
  };

  # this is a bit weird huh? Extract brainzo to a service at least.
  linux = {
    enable = true;
    blinds-controller = secrets.brainzo.blinds.controller;
    blinds = secrets.brainzo.blinds.blinds;
  };

  layers = {
    git = {
      enable = true;
      inherit emacs;
      hub-token = secrets.hub.token;
      name = secrets.user.name;
      email = secrets.user.email;
      github-user = "eddsteel";
      key = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
    };

    shell = {
      enable = true;
      inherit emacs;
      inherit (secrets.user) email;
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
      aws-configuration = secrets.aws.configuration;
      aws-credentials = secrets.aws.credentials;
    };

    emacs = {
      enable = true;
      package = emacs;
      local = secrets.emacs.local;
    };

    firefox = {
      enable = true;
      sync-user = secrets.user.email;
    };
  };
}

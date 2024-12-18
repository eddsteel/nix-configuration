{ config, pkgs, lib, ... }:
let
  username = "edd";
  homedir = "/home/${username}";
  emacs = pkgs.emacs29-pgtk;
  secrets = builtins.fromTOML (builtins.readFile ./secrets.toml);
in {
  imports = [ ../../modules/home ];

  home.stateVersion = "21.05";
  programs.home-manager = {
    enable = true;
    path = "${homedir}/src/home-manager";
  };

  home.packages = with pkgs; [
    calibre discord handbrake imagemagick ledger wavebox psmisc vlc wmctrl zoom-us
    tree-sitter
  ];

  local = {
    inherit homedir username;
    hostname = "draper";
    ssh = true;
  };

  services.brainzo = {
    enable = true;
    blinds-controller = secrets.brainzo.blinds.controller;
    blinds = secrets.brainzo.blinds.blinds;
  };

#  systemd.user.services = let
#    script = pkgs.writeShellScriptBin "nfs-save" ''
#      if [ $(ls -1 /mnt/nfs/books | wc -l) -le 0 ]; then
#        echo "unable to access NFS mounts, exiting."
#        exit
#      fi
#
#      ${pkgs.rsync}/bin/rsync -aHv /home/media/books/ /mnt/nfs/books/
#    '';
#    in {
#    nfs-save = {
#      Service = {
#        Type = "oneshot";
#        ExecStart = "${script}/bin/nfs-save";
#      };
#      Install = { WantedBy = ["default.target"]; };
#    };
#  };
#  systemd.user.timers.nfs-save = {
#    Install = {
#      WantedBy = [ "timers.target" ];
#    };
#    Timer = {
#      OnCalendar = "daily";
#    };
#  };

  layers = {
    linux = {
      enable = true;
      gnome = {
        enable = true;
        inherit homedir;
      };
    };

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

{ config, pkgs, lib, ... }:
let
  emacs = pkgs.emacs29;
  homedir = "/Users/edd";
  secrets =  builtins.fromTOML (builtins.readFile ./secrets.toml);
  nix-work = pkgs.callPackage ../../../nix-work {};
  work-pkgs = pkgs.callPackage (builtins.fetchGit {
    inherit (secrets.workpkgs) url ref rev;
  }) secrets.workpkgs.args;
in {
  imports = [ ../../modules/home/macos ../../modules/home ];

  home.stateVersion = "21.05";

  programs.home-manager = {
    enable = true;
    path = "${homedir}/src/home-manager";
  };

  home.packages = with pkgs; [
    scripts kotlin gradle terraform terraform-docs circleci-cli
    docker gettext zoom-us aws-vpn dos2unix kubectl kubectx bitwarden-cli
    pre-commit bat
  ] ++ nix-work.all ++ work-pkgs.all;

  programs.go.enable = true;

  local = {
    inherit homedir;
    username = "edd";
    hostname = "ringo";
  };

  # TODO: move these out.
  macos = {
    enable = true;
    prefs = {
      enable = true;
      preferences-files = [
        { domain = "com.googlecode.iterm2"; path = <nix-config> + "/files/com.googlecode.iterm2.plist"; }
      ];
    };
  };

  targets.darwin = {
    dock = {
      apps = [
        "/System/Applications/System Settings.app"
        "${emacs}/Applications/Emacs.app"
        "${pkgs.firefox}/Applications/Firefox.app"
        "${pkgs.iterm2}/Applications/iTerm2.app"
        "${pkgs.wavebox}/Applications/Wavebox.app"
        "${pkgs.bitwarden}/Applications/Bitwarden.app"
      ];
    };
    defaults = {
      "com.apple.dock" = {
        "autohide" = true;
        "region" = "CA";
        "tilesize" = 75;
        "show-recents" = false;
      };
      "com.apple.menuextra.clock" = {
        "IsAnalog" = true;
      };
      "com.apple.screencapture" = {
        "location" = "~/media/pictures";
      };
      "NSGlobalDomain" = {
        "AppleShowAllExtensions" = true;
      };
      "com.apple.Finder" = {
        "AppleShowAllFiles" = false;
      };
    };
  };

  istat = {
    enable = true;
    inherit (secrets.istat) email serial;
  };

  xbar.enable = true;
  skhd.enable = true;
  programs = {
    soundsource = {
      enable = true;
      inherit (secrets.soundsource) name code;
    };
  };

  git = {
    enable = true;
    inherit emacs;
    hub-token = secrets.hub.token;
    name = secrets.user.name;
    email = secrets.user.email;
    github-user = "eddsteel";
    key = "8433C6F9F807CE8E8DFA99EFB10455BC05772724";
  };

  shell = {
    enable = true;
    inherit emacs;
    inherit (secrets.user) email;
    extraAliases = {
      "s3" = "AWS_PROFILE=s3-dl-personal ${pkgs.scripts}/bin/s3";
      "gradle" = "envchain gradle gradle";
    } // secrets.shell.aliases;
  };

  workstation = {
    enable = true;
    github-name = "eddsteel";
    mr-repos = [
      {"name" = "git-web-link";}
      {"name" = "scripts";}
    ] ++ secrets.workstation.repos;
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
    sync-user = secrets.firefox.username;
  };
}

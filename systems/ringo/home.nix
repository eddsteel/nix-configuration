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
  imports = [ ../../modules/home ];

  home.stateVersion = "21.05";

  programs.home-manager = {
    enable = true;
    path = "${homedir}/src/home-manager";
  };

  home.packages = with pkgs; [
    scripts kotlin pre-commit bat gettext dos2unix
    terraform terraform-docs circleci-cli aws-vpn
    docker kubectl kubectx nixUnstable
  ] ++ nix-work.all
    ++ work-pkgs.all
    ++ (with mac-apps; [intellij-idea-ce caffeine orbstack]);

  programs.go.enable = true;

  local = {
    inherit homedir;
    username = "edd";
    hostname = "ringo";
    ssh = true;
  };

  layers = {
    macos = {
      enable = true;
      istat = {
        inherit (secrets.istat) email serial;
      };
      soundsource = {
        inherit (secrets.soundsource) name code;
      };
      inherit emacs;
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
  };

  programs.xbar = {
    enable = true;
    widgets = [
      {
        name = "emms-show";
        timer = 5;
        script = ''
            np=$(${pkgs.scripts}/bin/emms now-playing)
            echo "$np | size=13 length=50"
          '';
      }
      {
        name = "youbi";
        timer = 60;
        script = ''
            LC_ALL=ja_JP date +"%a曜日"
            echo '---'
            LC_ALL=ja_JP date
          '';
      }
      {
        name = "indoor-temp";
        timer = 60;
        script = ''
            PATH="$HOME/.nix-profile/bin"
            temp=$(nix-shell -p broadlink-cli --command 'broadlink_cli --type 0x5213 --host 192.168.1.162 --mac ec0baeee04b8 --temperature')
            printf "%s°C\n" $temp
          '';
      }
    ];
  };

  home.activation."refresh" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD killall Finder
 '';
}

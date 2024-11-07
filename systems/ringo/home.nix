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
    docker kubectl kubectx nixVersions.git
    podman wvlet
  ] ++ nix-work.all
    ++ work-pkgs.all
    ++ (with mac-apps; [intellij-idea-ce caffeine vfkit podman-desktop]);

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
      emacs = config.programs.emacs.finalPackage;
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
      profile = "uooem51c.default-release-5";
    };

    git = {
      enable = true;
      inherit emacs;
      hub-token = secrets.hub.token;
      name = secrets.user.name;
      email = secrets.user.email;
      github-user = "eddsteel";
      key = "8433C6F9F807CE8E8DFA99EFB10455BC05772724";
      jj = true;
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
            temp=$(nix-shell -p broadlink-cli --command 'broadlink_cli --type 0x5213 --host ${secrets.temperature.host} --mac ${secrets.temperature.mac} --temperature')
            printf "%s°C\n" $temp
            echo '---'
            echo "History | href=${secrets.temperature.url}"
          '';
      }
    ];
  };

  home.activation."zzzRefresh" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    echo $0
    run killall Finder
 '';
}

{ config, pkgs, lib, ... }:
let
  devices = import ../devices.nix { };
  sources = import ../../npins;
  emacs = pkgs.emacs30;
  homedir = "/Users/edward";
  secrets =  builtins.fromTOML (builtins.readFile ./secrets.toml);
  nix-work = import sources.nix-work {};
  work-pkgs = pkgs.callPackage (builtins.fetchGit {
    inherit (secrets.workpkgs) url ref rev;
  }) secrets.workpkgs.args;
  hosts = import ../hosts.nix { inherit lib; };
  paneru = builtins.getFlake "github:karinushka/paneru";
in {
  imports = [
    ../../modules/home
    paneru.homeModules.paneru
  ];

  home.stateVersion = "21.05";

  programs.home-manager = {
    enable = true;
    path = sources.home-manager.outPath;
  };

  home.packages = with pkgs; [
    npins scripts kotlin kotlin-interactive-shell gettext dos2unix pre-commit
    terraform-docs circleci-cli aws-vpn docker kubectl kubectx
    nixVersions.git podman trino maven claude-code jdk21 ruby opentofu
] ++ nix-work.all
  ++ work-pkgs.all
  ++ (with mac-apps; [caffeine vfkit podman-desktop intellij-idea-ce]);

  programs.go.enable = true;

  local = {
    inherit homedir;
    username = "edward";
    hostname = "george";
    ssh = true;
  };

  services.paneru = {
    enable = true;
    settings = {
      bindings = {
        window_focus_north       = "cmd - uparrow";
        window_focus_south       = "cmd - downarrow";
        window_focus_west        = "cmd - leftarrow";
        window_focus_east        = "cmd - rightarrow";
        window_focus_first       = "cmd - home";
        window_focus_last        = "cmd - end";
        window_virtual_north     = "cmd - pageup";
        window_virtual_south     = "cmd - pagedown";
        window_swap_north        = "cmd + shift - uparrow";
        window_swap_south        = "cmd + shift - downarrow";
        window_swap_west         = "cmd + shift - leftarrow";
        window_swap_east         = "cmd + shift - rightarrow";
        window_swap_first        = "cmd + shift - home";
        window_swap_last         = "cmd + shift - end";
        window_virtualmove_north = "cmd + shift - pageup";
        window_virtualmove_south = "cmd + shift - pagedown";
        window_grow              = "cmd + ctrl - uparrow";
        window_shrink            = "cmd + ctrl - downarrow";
        window_stack             = "cmd + ctrl - leftarrow";
        window_unstack           = "cmd + ctrl - rightarrow";
        window_center            = "cmd + ctrl - home";
        window_fullwidth         = "cmd + ctrl - end";
        window_virtualsend_north = "cmd + ctrl - pageup";
        window_virtualsend_south = "cmd + ctrl - pagedown";
        window_manage            = "cmd + ctrl - m";
        window_equalize          = "cmd + ctrl - equal";
        window_snap              = "cmd + ctrl - minus";
      };
      options = {
        focus_follows_mouse = true;
        mouse_follows_focus = true;
        preset_column_widths = [0.25 0.33 0.5 0.66 0.75 1.0];
        animation_speed = 12;
      };
      swipe.scroll.modifier = "ctrl + alt + shift + cmd"; # i.e. hyper
    };
  };

  layers = {
    home = {
      device = "${secrets.home.device-controller} ${hosts.ip4 "controller"} ${lib.replaceStrings ["-"] [""] (lib.strings.toLower (hosts.mac "controller"))}";
      blind-controller = secrets.home.blind-controller;
      blinds = devices.blinds;
      packets = devices.packets;
    };

    macos = {
      enable = true;
      istat = {
        inherit (secrets.istat) email serial;
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
      profile = "so1243a1.default-release";
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
      inherit (secrets.shell) vars funs;
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
            temp=$(roomtemp)
            printf "%s°C\n" $temp
            echo '---'
            echo "History | href=${secrets.temperature.url}"
          '';
      }
    ];
  };

  home.file.".wvlet/profiles.yml".source = (pkgs.formats.yaml {}).generate "wvlet-config" {
    profiles = secrets.wvlet.config;
  };

  home.file.".config/ldcli/config.yml".source = (pkgs.formats.yaml {}).generate "ldcli-config" secrets.launch-darkly;
  home.file.".m2/settings.xml".text = let
    servers = with builtins; concatStringsSep "\n" (map serverString secrets.maven.server);
    serverString = o: ''
      <server>
        <id>${o.id}</id>
        <username>${o.username}</username>
        <password>${o.password}</password>
      </server>
    '';
    in ''
      <?xml version='1.0' encoding='utf-8'?>
      <settings>
        <servers>
          ${servers}
        </servers>
      </settings>
    '';

  home.activation."zzzRefresh" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    echo $0
    run killall Finder
 '';
}

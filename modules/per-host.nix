{ lib, config, ... }:
with lib;
let
  cfg = config.perHost;
  hostName = config.networking.hostName;
in {
  options.perHost = {
    enable = mkEnableOption "Use per-host configuration";
    os = mkOption {
      type = types.str;
      default = "nixos";
      description = "nixos or darwin";
    };
  };

  config = let
    configPath = toString <nix-config>;
    nixpkgsPath = toString <nixpkgs>;
    pkgsconfig = "${configPath}/nixpkgs.nix";
    overlaysconfig = "${configPath}/overlays";
    localRoot = "${configPath}/systems/${hostName}";
    osconfig = "${localRoot}/${cfg.os}.nix";
    homeconfig = "${localRoot}/home.nix";
  in mkIf cfg.enable {
    nix.nixPath = [
      "${cfg.os}-config=${osconfig}"
      "hm-config=${homeconfig}"
      "nixpkgs=${nixpkgsPath}" # ∞ set before we customise config
      "nixpkgs-config=${pkgsconfig}"
      "nixpkgs-overlays=${overlaysconfig}"
      "nix-config=${configPath}" # ∞ set at the command line, once
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    environment = let
      envvars = {
        HOSTNAME = hostName;
        HOME_MANAGER_CONFIG = homeconfig;
        NIX_CONF_DIR = configPath;
      };
    in if cfg.os == "darwin"
      then {
        variables = envvars // {
          NIXPKGS_CONFIG = pkgsconfig;  # It's an error to configure this in nixos 🙄
        };
        darwinConfig = osconfig;
      }
      else {
        variables = envvars // {
          NIXOS_CONFIG = osconfig;
        };
        etc."nix/nixpkgs-config.nix".source = pkgsconfig; # Just overwrite it instead 😈
      };

      nix.channel.enable = false;
  };
}

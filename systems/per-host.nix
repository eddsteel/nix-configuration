{ lib, config, ... }:
with lib;
let
  cfg = config.perHost;
in {
  options.perHost = {
    enable = mkEnableOption "Use per-host configuration";
    hostName = mkOption {
      type = types.str;
    };
    os = mkOption {
      type = types.str;
      default = "nixos";
      description = "nixos or darwin";
    };
  };

  config = let
    configPath = toString <nix-config>;
    nixpkgsPath = toString <nixpkgs>;
    localRoot = "${configPath}/systems/${cfg.hostName}";
    osconfig = "${localRoot}/${cfg.os}.nix";
    homeconfig = "${localRoot}/home.nix";
    pkgsconfig = "${configPath}/nixpkgs.nix";
    overlaysconfig = "${configPath}/overlays.nix";
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

    nixpkgs.overlays = import "${overlaysconfig}";

    environment = let
      envvars = {
        HOSTNAME = cfg.hostName;
        HOME_MANAGER_CONFIG = homeconfig;
      };
    in if cfg.os == "darwin"
      then {
        variables = envvars // {
          NIXPKGS_CONFIG = pkgsconfig;
        }; # It's an error to configure this in nixos 🙄
        darwinConfig = osconfig;
      }
      else {
        variables = envvars // {
          NIXOS_CONFIG = osconfig;
        };
      };
  };
}

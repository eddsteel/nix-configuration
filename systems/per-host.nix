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
    user = mkOption {
      type = types.str;
      default = "root";
      description = "User (required for darwin)";
    };
  };

  config = let
    configPath = toString <nix-config>;
    localRoot = "${configPath}/systems/${cfg.hostName}";
    osconfig = "${localRoot}/${cfg.os}.nix";
    homeconfig = "${localRoot}/home.nix";
    pkgsconfig = "${configPath}/nixpkgs.nix";
  in mkIf cfg.enable {
    nix.nixPath = [
      "${cfg.os}-config=${osconfig}"
      "hm-config=${homeconfig}"
      "nixpkgs=/nix/var/nix/profiles/per-user/${cfg.user}/channels/${cfg.os}"
      "nixpkgs-config=${pkgsconfig}"
      "nixpkgs-overlays=${configPath}/overlays.nix"
      "nix-config=${configPath}" # ∞
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    nixpkgs.overlays = import "${configPath}/overlays.nix";

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

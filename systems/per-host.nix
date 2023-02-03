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
    configPath = mkOption {
      type = types.str; #Paths would be copied into the store.
      default = "/etc/${cfg.os}";
    };
  };

  config = let
    localRoot = "${cfg.configPath}/systems/${cfg.hostName}";
    osconfig = "${localRoot}/${cfg.os}.nix";
    homeconfig = "${localRoot}/home.nix";
    pkgsconfig = "${cfg.configPath}/nixpkgs.nix";
  in mkIf cfg.enable {
    nix.nixPath = (if cfg.os == "nixos" then [
      { nixos-config = osconfig; }
    ] else []) ++ [
      "hm-config=${homeconfig}"
      "nixpkgs=/nix/var/nix/profiles/per-user/${cfg.user}/channels/${cfg.os}"
      "/nix/var/nix/profiles/per-user/root/channels"
      { nixpkgs-overlays = "${cfg.configPath}/overlays.nix";}
    ];

    nixpkgs.overlays = import "${cfg.configPath}/overlays.nix";

    environment = let
      envvars = {
        HOSTNAME = cfg.hostName;
        HOME_MANAGER_CONFIG = homeconfig;
        NIXPKGS_CONFIG = pkgsconfig;
      };
    in if cfg.os == "darwin"
      then {
        variables = envvars;
        darwinConfig = osconfig;
      }
      else {
        variables = envvars // {
          NIXOS_CONFIG = osconfig;
        };
      };
  };
}

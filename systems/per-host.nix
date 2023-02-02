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
    configPath = mkOption {
      type = types.path;
      default = "/etc/${cfg.os}";
    };
  };

  config = mkIf cfg.enable {
    networking.hostName = cfg.hostName;

    nix.nixPath = [
      "${cfg.os}-config=${cfg.configPath}/systems/${cfg.hostName}/${cfg.os}.nix"
      "hm-config=${cfg.configPath}/systems/${cfg.hostName}/home.nix"
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    environment.variables = {
      HOSTNAME = cfg.hostName;
      NIXOS_CONFIG = "${cfg.configPath}/nixpkgs/systems/${cfg.hostName}/${cfg.os}.nix";
      HOME_MANAGER_CONFIG = "${cfg.configPath}/nixpkgs/systems/${cfg.hostName}/home.nix";
    };
  };
}

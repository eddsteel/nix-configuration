{ lib, config, ... }:
with lib;
let
  sources = import ../npins;
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
    channels = mkOption {
      type = types.bool;
      default = false;
      description = "whether to use channels (default false)";
    };
  };

  config = let
    configPath = toString <nix-config>;
    pkgsconfig = "${configPath}/nixpkgs.nix";
    overlaysconfig = "${configPath}/overlays";
    localRoot = "${configPath}/systems/${hostName}";
    osconfig = "${localRoot}/${cfg.os}.nix";
    homeconfig = "${localRoot}/home.nix";
  in mkIf cfg.enable {
    nix.nixPath = [
      "home-manager=${sources.home-manager}"
      "nixpkgs=${sources.nixpkgs}"
      "${cfg.os}-config=${osconfig}"
      "hm-config=${homeconfig}"
      "nixpkgs-config=${pkgsconfig}"
      "nixpkgs-overlays=${overlaysconfig}"
      "nix-config=${configPath}" # ∞ set at the command line, once
    ] ++  (lib.lists.optionals (cfg.os == "darwin") [ "darwin=${sources.darwin}"]);

    environment = let
      envvars = {
        HOSTNAME = hostName;
        HOME_MANAGER_CONFIG = homeconfig;
        NIX_CONF_DIR = configPath;
      };
    in if cfg.os == "darwin"
       then {
         darwinConfig = osconfig;
         variables = envvars // {
           NIXPKGS_CONFIG = pkgsconfig;  # It's an error to configure this in nixos 🙄
         };
       }
       else {
         variables = envvars // {
           NIXOS_CONFIG = osconfig;
         };
         etc."nix/nixpkgs-config.nix".source = pkgsconfig; # Just overwrite it instead 😈
       };

      nix.channel.enable = cfg.channels;
  };
}

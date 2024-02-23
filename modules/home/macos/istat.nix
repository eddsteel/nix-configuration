{config, pkgs, lib, ...}:
let cfg = config.programs.istat;
in with lib; {
  options.programs.istat = {
    enable = mkEnableOption "IStatMenus with registration and options";
    serial = mkOption {};
    email = mkOption {};
    extras = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mac-apps.istat-menus];

    targets.darwin.defaults = {
      "com.bjango.istatmenus".license6 = {
        email = cfg.email;
        serial = cfg.serial;
      };
      "com.bjango.istatmenus6.extras" = cfg.extras;
    };
  };
}

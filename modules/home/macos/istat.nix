{config, pkgs, lib, ...}:
let cfg = config.istat;
in with lib; {
  options.istat = {
    enable = mkEnableOption "My Istat registration";
    serial = mkOption {};
    email = mkOption {};
    license5 = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mac-apps.istat-menus];

    home.activation."istatRegistration" = hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD defaults write com.bjango.istatmenus license6 -dict email "${cfg.email}" serial "${cfg.serial}"
    $DRY_RUN_CMD defaults import com.bjango.istatmenus6.extras ${../../../files}/com.bjango.istatmenus6.extras.plist
    '';
  };
}

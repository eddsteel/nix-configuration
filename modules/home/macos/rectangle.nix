{config, pkgs, lib, ...}:
let cfg = config.programs.rectangle;
in with lib; {
  options.programs.rectangle = {
    enable = mkEnableOption "Rectangle for macos";
    preferences = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [ pkgs.rectangle ];
    targets.darwin.defaults."com.knollsoft.Rectangle" = cfg.preferences;
  };
}

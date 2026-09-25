{config, pkgs, lib, ...}:
let cfg = config.programs.karabiner;
in with lib; {
  options.programs.karabiner = {
    enable = mkEnableOption "Karabiner for macos";
    settings-file = mkOption {};
  };
  config = mkIf cfg.enable {
    xdg.configFile."karabiner/karabiner.json".source = cfg.settings-file;
  };
}

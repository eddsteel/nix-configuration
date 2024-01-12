{config, pkgs, lib, ...}:
let cfg = config.programs.soundsource;
in with lib; {
  options.programs.soundsource = {
    enable = mkEnableOption "My SoundSource registration";
    code = mkOption { type = types.str; };
    name = mkOption { type = types.str; };
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mac-apps.soundsource];
    targets.darwin.defaults."com.rogueamoeba.soundsource" = {
      versionChecking = 0;
      registrationInfo = { Code = cfg.code; Name = cfg.name;};
    };
  };
}

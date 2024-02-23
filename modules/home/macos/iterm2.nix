{config, pkgs, lib, ...}:
let cfg = config.programs.iterm2;
in with lib; {
  options.programs.iterm2 = {
    enable = mkEnableOption "Iterm 2";
    workingdir = mkOption {};
    preferences = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [ pkgs.iterm2 ];

    targets.darwin.defaults."com.googlecode.iterm2" = cfg.preferences // {
      "Working Directory" = "${cfg.workingdir}";
    };
  };
}

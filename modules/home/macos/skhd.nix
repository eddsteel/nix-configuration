{config, pkgs, lib, ...}:
let cfg = config.programs.skhd;
in with lib; {
  options.programs.skhd = {
    enable = mkEnableOption "SKHD configuration";
    bindings = mkOption { type = with types; attrsOf str; };
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.skhd];

    xdg.configFile."skhd/skhdrc".text =
      lib.strings.concatStringsSep "\n" (lib.attrsets.mapAttrsToList (k: b: "${k}: ${b}") cfg.bindings)
    ;

    launchd.agents."com.koekishiya.skhd" = {
      enable = true;
      config = {
        Label = "com.koekeishiya.skhd";
        ProgramArguments = ["${pkgs.skhd}/bin/skhd"];
        EnvironmentVariables = {
          "PATH" = builtins.getEnv "PATH";
        };
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/tmp/skhd.out";
        StandardErrorPath = "/tmp/skhd.err";
      };
    };
  };
}

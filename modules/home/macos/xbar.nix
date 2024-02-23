{config, pkgs, lib, ...}:
let cfg = config.programs.xbar;
    mkXbarScript = {name, timer, script}: {
      "Library/Application Support/xbar/plugins/${name}.${builtins.toString timer}.sh" = {
        text = ''#!${pkgs.bash}/bin/bash
        ${script}
        '';
        executable = true;
      };
    };
in with lib; {
  options.programs.xbar = {
    enable = mkEnableOption "XBar";
    widgets = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mac-apps.xbar];
    home.file = lib.attrsets.mergeAttrsList (builtins.map mkXbarScript cfg.widgets);
  };
}

{config, pkgs, lib, ...}:
let cfg = config.services.brainzo;
in with lib; {
  options.services.brainzo = {
    enable = mkEnableOption "Brainzo";
    blinds-controller = mkOption {};
    blinds = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.brainzo];
    systemd.user.services.brainzo-api = {
      Unit = { Description = "brainzo-api"; };
      Install = { WantedBy = ["default.target"]; };
      Service = {
        Type = "simple";
        Restart = "always";
        ExecStart = "${pkgs.brainzo}/bin/brainzo-api";
        KillMode = "process";
        TimeoutSec = 180;
      };
    };
    xdg.configFile."brainzo/brainzo.conf".text = ''
[BLINDS]
host   = blinds
port   = 8838
id     = ${cfg.blinds-controller}
blinds = ${cfg.blinds}

[RADIO]
stations = 1xtra http://stream.live.vc.bbcmedia.co.uk/bbc_1xtra
           bbc3  http://stream.live.vc.bbcmedia.co.uk/bbc_radio_three
'';
  };
}

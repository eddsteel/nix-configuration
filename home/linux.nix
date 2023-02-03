{pkgs, config, localPkgs, ...} :
{
  programs.gpg = {
    enable = true;
    settings = {
      "require-cross-certification" = true;
      "keyserver" = ["hkp://keys.gnupg.net" "http://http-keys.gnupg.net"];
      "keyserver-options" = " auto-key-retrieve";
      "use-agent" = true;
    };
  };
  services.gpg-agent = {
    enable = true;
    extraConfig = ''
    default-cache-ttl 3600
    allow-emacs-pinentry
    '';
  };

  systemd.user = {
    tmpfiles.rules = [
      "e ${config.home.homeDirectory}/tmp                0755 edd users 10d -"
    ];
    startServices = true;

#    services.brainzo-api = {
#      Unit = { Description = "brainzo-api"; };
#      Install = { WantedBy = ["default.target"]; };
#      Service = {
#        Type = "simple";
#        Restart = "always";
#        ExecStart = "${pkgs.brainzo}/bin/brainzo-api";
#        KillMode = "process";
#        TimeoutSec = 180;
#      };
#    };
  };

 # home.packages = [pkgs.brainzo];

  xdg.configFile."geary/account_01/geary.ini".source = ../files/geary.ini;
}

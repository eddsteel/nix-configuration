{pkgs, lib, config, localPkgs, ...} :
let cfg = config.layers.linux;
in with lib; {
  options.layers.linux = {
    enable = mkEnableOption "Linux-specific settings";
  };

  config = mkIf cfg.enable {
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
    };
    home.file.".desktop.jpg".source = pkgs.fetchurl {
      name = "desktop.jpg";
      url = "https://eddsteel.com/desktop.jpg";
      sha256 = "101mavys8azcah35f1cxiiblrbja5vwrf7kiczgv2byd0ymx9nhz";
    };
    home.file.".face".source = pkgs.fetchurl {
      url = "https://eddsteel.com/face.jpg";
      name = "face.jpg";
      sha256 = "1y6hp0n203ccgb2a248xa3i2niflj5wxbd40q69c3p7qd79x3405";
    };
  };
}

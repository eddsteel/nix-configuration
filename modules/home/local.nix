{ lib, config, ... }:
with lib;
let
  cfg = config.local;
in {
  options.local = {
    username = mkOption {};
    homedir = mkOption {};
    hostname = mkOption {};
    ssh = mkOption { default = true; };
  };

  config = mkMerge [
    {
      home.username = cfg.username;
      home.homeDirectory = cfg.homedir;
    }

    (mkIf cfg.ssh {
      home.file.".ssh/id_rsa.pub".source =  "${../../files}/id_rsa.${cfg.username}.${cfg.hostname}.pub";
      home.file.".ssh/id_rsa".source = "${../../secrets}/id_rsa.${cfg.username}.${cfg.hostname}";

      programs.ssh = {
        enable = true;
        extraConfig = ''
      Host *
        IgnoreUnknown UseKeychain
        UseKeychain yes
        AddKeysToAgent yes
        IdentityFile ~/.ssh/id_rsa
      '';
      };
    })
  ];
}

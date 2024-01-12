{ lib, config, ... }:
with lib;
let
  cfg = config.local;
  ssh-stub = "id_rsa.${cfg.username}.${cfg.hostname}";
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
      home.file.".ssh/id_rsa.pub".source =  "${<nix-config>}/files/${ssh-stub}.pub";
      home.file.".ssh/id_rsa".source = "${<nix-config>}/keys/${ssh-stub}";

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

{ lib, config, ... }:
with lib;
let
  cfg = config.local;
  people = import ../../systems/people.nix { inherit lib; };
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
      home.file.".ssh/id_rsa.pub".text = with cfg; people.pubkey username hostname;
      home.file.".ssh/id_rsa".text = with cfg; people.seckey username hostname;
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

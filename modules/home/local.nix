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
        enableDefaultConfig = false;
        matchBlocks = {
          "*" = {
            addKeysToAgent = "yes";
            compression = false;
            controlMaster = "yes";
            controlPath = "~/.ssh/master-%r@%n:%p";
            hashKnownHosts = false;
            identityFile = "~/.ssh/id_rsa";
            serverAliveCountMax = 3;
            serverAliveInterval = 0;
            userKnownHostsFile = "~/.ssh/known_hosts";
            extraOptions = {
              UseKeychain = "yes";
              IgnoreUnknown = "UseKeychain";
            };
          };
          "github.com" = {
            user = "eddsteel";
            addKeysToAgent = "yes";
            extraOptions = {
              # UseKeychain = "yes";
            };
          };
        };
      };
    })
  ];
}

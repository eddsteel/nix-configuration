{ lib, config, ... }:
with lib;
let
  cfg = config.local;
in {
  options.local = {
    username = mkOption {};
    homedir = mkOption {};
    hostname = mkOption {};
  };

  config = {
    home.username = cfg.username;
    home.homeDirectory = cfg.homedir;

    home.file.".ssh/id_rsa.pub".source = "${./files}/id_rsa.${cfg.username}.${cfg.hostname}.pub";
    home.file.".ssh/id_rsa".source = "${./secrets}/id_rsa.${cfg.username}.${cfg.hostname}";
  };
}

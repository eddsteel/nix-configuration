{ lib, config, ... }:
with lib;
let
  username = config.home.username;
  homedir = config.home.homeDirectory;
  hostname = builtins.getEnv "HOSTNAME";
in {
  config = {
    home.file.".ssh/id_rsa.pub".source = "${./files}/id_rsa.${username}.${hostname}.pub";
    home.file.".ssh/id_rsa".source = "${./secrets}/id_rsa.${username}.${hostname}";
    shell.homeConfig = "${homedir}/.config/nixpkgs/systems/${hostname}/home.nix";
  };
}

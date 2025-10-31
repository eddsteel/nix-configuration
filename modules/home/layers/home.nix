# todo: bring in temperature script.
{pkgs, lib, config, ... }:
with lib;
let
  cfg = config.layers.home;
  packetsFiles = lib.attrsets.concatMapAttrs (k: v: { "homeauto/packets/${k}".text = v; }) cfg.packets;
  bcs = pkgs.writeShellScriptBin "bcs" ''
     ${pkgs.broadlink-cli}/bin/broadlink_cli --device @$HOME/.config/homeauto/home.device --send @$HOME/.config/homeauto/packets/$1
  '';
in {
  options.layers.home = {
    device = mkOption {};
    packets = mkOption {};
  };

  config = {
    xdg.configFile = {
      "homeauto/home.device".text = cfg.device;
    } // packetsFiles;

    home.packages = with pkgs; [ broadlink-cli bcs ];
  };
}

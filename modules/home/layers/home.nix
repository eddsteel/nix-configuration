{pkgs, lib, config, ... }:
with lib;
let
  cfg = config.layers.home;
  packetsFiles = lib.attrsets.concatMapAttrs (k: v: { "homeauto/packets/${k}".text = v; }) cfg.packets;
  bcs = pkgs.writeShellScriptBin "bcs" ''
     ${pkgs.broadlink-cli}/bin/broadlink_cli --device @$HOME/.config/homeauto/home.device --send @$HOME/.config/homeauto/packets/$1
  '';
  roomtemp = pkgs.writeShellScriptBin "roomtemp" ''
    ${pkgs.broadlink-cli}/bin/broadlink_cli --device @$HOME/.config/homeauto/home.device --temperature
  '';
  blind = pkgs.writeShellScriptBin "blind" ''
    id="${cfg.blind-controller}"
    hash=$(cat /dev/urandom | tr -cd '[:digit:]' | head -c 7)
    blind=$(echo "${cfg.blinds}" | cut -d' ' -f $1)
    cmd=$2

    ${pkgs.curl}/bin/curl \
      "http://blinds:8838/neo/v1/transmit?id=$id&command=$blind-$cmd&hash=$hash"
  '';
  kino = pkgs.writeShellScriptBin "kino" ''
    ${blind}/bin/blind 1 dn
    ${blind}/bin/blind 2 dn
    ${blind}/bin/blind 3 dn
    ${blind}/bin/blind 4 dn
    ${blind}/bin/blind 5 gp  # use favourite not down so it doesn't hit the cat
    ${blind}/bin/blind 6 dn
    ${bcs}/bin/bcs projector/on
    ${bcs}/bin/bcs fan/light
    sleep 1;
    ${bcs}/bin/bcs lamp/off
    ${bcs}/bin/bcs speakers/power
    sleep 1;
    ${bcs}/bin/bcs speakers/bt
  '';
in {
  options.layers.home = {
    device = mkOption {};
    packets = mkOption {};
    blinds = mkOption {};
    blind-controller = mkOption {};
  };

  config = {
    xdg.configFile = {
      "homeauto/home.device".text = cfg.device;
    } // packetsFiles;

    home.packages = with pkgs; [ broadlink-cli bcs roomtemp blind curl kino];
  };
}

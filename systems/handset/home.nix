{ config, pkgs, lib, ...}:
let
  username = "nix-on-droid";
  homedir = "/data/data/com.termux.nix/files/home";
  hosts  = import ../hosts.nix { inherit lib; }
in {
  imports = [ ../modules/home ];
  home.stateVersion = "21.05";

  programs.home-manager = {
    enable = true;
    path = "${homedir}/home-manager"
  };

  home.packages = with pkgs; [ git fish ];

  local = {
    inherit homedir username;
    hostname = "handset";
    ssh = true;
  };
}

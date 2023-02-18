# Use this to generate self-signed certs for local network consul.
# Then copy them out of the store.
{ pkgs ? import <nixpkgs> {} }:
let hosts = pkgs.callPackage ../../systems/hosts.nix {};
in pkgs.callPackage ./consul-cert.nix {
  inherit (hosts) domain consulMain;
}

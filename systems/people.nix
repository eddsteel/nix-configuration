{ lib }:
rec {
  pubkey = user: host: builtins.readFile (./. + "/${host}/id_rsa.${user}.pub");
  seckey = user: host: builtins.readFile (<nix-config> + "/keys/id_rsa.${user}.${host}");
  pubkeys = [(pubkey "edd" "ringo") (pubkey "edd" "draper")];
}

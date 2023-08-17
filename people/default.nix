{ lib }:
let
  stub = u: h: "id_rsa.${u}.${h}";
in rec {
  pubkey = user: host: builtins.readFile (./. + "/${(stub user host)}.pub");
  seckey = user: host: builtins.readFile (<secrets> + "/${(stub user host)}");
  pubkeys = [
    pubkey "edd" "ringo"
    pubkey "edd" "gusting"
    pubkey "edd" "da-shi"
  ];
}

{ pkgs ? import <nixpkgs> {} }:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).caffeine;
in pkgs.stdenv.mkDerivation rec {
  pname = "signal";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Caffeine.app $out/Applications/Caffeine.app
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "Caffeine";
    homepage = "https://intelliscapesolutions.com/apps/caffeine";
    maintainers = [ (import ../../maintainers.nix).eddsteel ];
    platforms = platforms.darwin;
  };
}

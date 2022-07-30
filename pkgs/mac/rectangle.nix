{ pkgs ? import <nixpkgs> {} }:
let
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).rectangle;
in pkgs.stdenv.mkDerivation rec {
  pname = "rectangle";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Rectangle.app "$out/Applications/Rectangle.app"
      '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "Rectangle";
    homepage = "https://www.rectangleapp.com";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

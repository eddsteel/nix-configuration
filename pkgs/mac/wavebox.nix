{ pkgs ? import <nixpkgs> {} }:
let
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).wavebox;
in pkgs.stdenv.mkDerivation rec {
  pname = "wavebox";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Wavebox.app "$out/Applications/Wavebox.app"
      '';

  src = pkgs.fetchurl {inherit (versions) name url sha256;};

  meta = with pkgs.lib; {
    description = "Wavebox";
    homepage = "https://wavebox.io";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.3.10";
  sha = "03v1vh053pfwvhggn34yffsil375b96syfjp3zsd61yriy3sha4n";

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r SoundSource.app $out/Applications
  '';

  src = pkgs.fetchurl {
    name = "soundsource-${version}.zip";
    url = https://rogueamoeba.com/soundsource/download/SoundSource.zip;
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "SoundSource";
    homepage = https://rogueamoeba.com/soundsource;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

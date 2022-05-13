{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.3.11";
  sha = "10dzimgrprmwvp7j3hjf6l31jz4l9vnjjdz83q51h4h07fgwqbs7";

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

{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.5.4";
  sha = "02ndwaq1xgcln9hkvrbl3gfynf7m4pbj6n30vad53nsdbrrl4ck6";

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

{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.8.3";
  sha256 = "sha256-tomlsji65xVPTqX36UKf7S04M1iBxsXVJFEFqWPgBac=";

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
    inherit sha256;
  };

  meta = with pkgs.lib; {
    description = "SoundSource";
    homepage = https://rogueamoeba.com/soundsource;
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

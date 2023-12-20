{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.6.3";
  sha256 = "sha256-uXQw4MEV4hkrd7tjNCxtuXpbfmdW8bilI5ZmXwn9BLM=";

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

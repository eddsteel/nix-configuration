{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.8.6";
  sha256 = "sha256-ok0TNREpG+/lVS8W/b2Ce4EB3vYBPNX5hnE8LvCdxDQ=";

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

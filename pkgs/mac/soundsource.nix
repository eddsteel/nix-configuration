{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "soundsource";
  version = "5.6.0";
  sha = "1avm1jr75mjbps0fad3glshrwl42vnhc0f9sak038ny85f3apyi0";

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
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

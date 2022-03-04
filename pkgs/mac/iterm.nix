{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "iterm";
  version = "3.4.15";
  versionString = pkgs.lib.strings.replaceStrings ["."] ["_"] version;
  sha = "0y0vhxwn1cl0y1gjm5fad5zndb4v448mqcksbmmskpgg73h4wn9j";

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r iTerm.app "$out/Applications/iTerm2.app"
      '';

  src = pkgs.fetchurl {
    name = "iterm2-${version}.zip";
    url = "https://iterm2.com/downloads/stable/iTerm2-${versionString}.zip";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "iTerm 2";
    homepage = "https://iterm2.com";
    license = licenses.gpl2Only;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

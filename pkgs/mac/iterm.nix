{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "iterm2";
  version = "3.4.16";
  versionString = pkgs.lib.strings.replaceStrings ["."] ["_"] version;
  sha = "02wwskb6qqg303qz7l8fwfscp6l4kf5njds9kw76i7xdiq01m55h";

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

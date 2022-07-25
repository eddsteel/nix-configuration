{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "signal";
  version = "5.51.0";
  sha = "sha256-4xFIbGfJTgh+9R3pbm/1zM08b3EtldaExvIotj7META=";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Signal.app $out/Applications/Signal.app
  '';

  src = pkgs.fetchurl {
    name = "signal-${version}.dmg";
    url = "https://updates.signal.org/desktop/signal-desktop-mac-universal-${version}.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Signal messenger";
    homepage = "https://signal.org";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

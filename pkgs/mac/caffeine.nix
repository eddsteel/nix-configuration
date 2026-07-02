{ pkgs ? import <nixpkgs> {} }:
let
  src = (import ../../npins).caffeine;
  version = "1.1.4";
in pkgs.stdenv.mkDerivation rec {
  pname = "caffeine";
  inherit src version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Caffeine.app $out/Applications/Caffeine.app
  '';

  meta = with pkgs.lib; {
    description = "Caffeine";
    homepage = "https://intelliscapesolutions.com/apps/caffeine";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

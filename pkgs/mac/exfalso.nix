{ pkgs }:
let
  src = (import ../../npins).exfalso;
  version = "4.4.0";
in pkgs.stdenv.mkDerivation rec {
  pname = "exfalso";
  inherit version src;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r ExFalso.app "$out/Applications/ExFalso.app"
      '';

  meta = with pkgs.lib; {
    description = "ExFalso";
    homepage = "https://quodlibet.github.io";
    license = licenses.gpl2Only;
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

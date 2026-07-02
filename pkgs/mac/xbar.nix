{ pkgs }:
let
  src = (import ../../npins).xbar;
  version = builtins.head (builtins.match ".*xbar.v(.*).dmg" src.url);
in pkgs.stdenv.mkDerivation rec {
  inherit version src;
  pname = "xbar";
  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r xbar.app "$out/Applications/xbar.app"
      '';
  meta = with pkgs.lib; {
    description = "xbar";
    homepage = "https://xbarapp.com";
    license = licenses.mit;
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

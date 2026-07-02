{ pkgs ? import <nixpkgs> {}}:
let
  src = (import ../../npins).istat;
  version = builtins.head (builtins.match "https://.*istatmenus(.*).zip" src.url);
in pkgs.stdenv.mkDerivation rec {
  pname = "istat-menus";
  inherit version src;

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r "iStat Menus.app" $out/Applications/iStatMenus.app
  '';

  meta = with pkgs.lib; {
    description = "iStat Menus 6, Mac system monitor for your menubar";
    homepage = "https://bjango.com/mac/istatmenus";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "istat-menus";
  version = "6.62";
  sha = "sha256-D7984pb1dFZFPlI7PXQVI6TaXDeDuPtQMM7A/bdz18I=";

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r "iStat Menus.app" $out/Applications
  '';

  src = pkgs.fetchurl {
    name = "iStat-Menus-${version}.zip";
    url = "https://cdn.bjango.com/files/istatmenus6/istatmenus${version}.zip";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "iStat Menus 6, Mac system monitor for your menubar";
    homepage = "https://bjango.com/mac/istatmenus";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

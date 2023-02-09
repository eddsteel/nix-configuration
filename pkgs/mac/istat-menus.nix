{ pkgs ? import <nixpkgs> {}}:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).istatmenus;
in pkgs.stdenv.mkDerivation rec {
  pname = "istat-menus";
  inherit (versions) version;

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r "iStat Menus.app" $out/Applications/iStatMenus.app
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "iStat Menus 6, Mac system monitor for your menubar";
    homepage = "https://bjango.com/mac/istatmenus";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

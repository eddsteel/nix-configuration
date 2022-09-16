{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).exfalso;
in pkgs.stdenv.mkDerivation rec {
  pname = "exfalso";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r ExFalso.app "$out/Applications/ExFalso.app"
      '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "ExFalso";
    homepage = "https://quodlibet.github.io";
    license = licenses.gpl2Only;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).signal;
in pkgs.stdenv.mkDerivation rec {
  pname = "signal";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Signal.app $out/Applications/Signal.app
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "Signal messenger";
    homepage = "https://signal.org";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

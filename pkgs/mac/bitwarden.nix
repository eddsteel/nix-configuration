{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).bitwarden;
in  pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  inherit (versions) version;
  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Bitwarden.app $out/Applications
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "Bitwarden password manager";
    homepage = "https://bitwarden.com";
    maintainers = [ (import ../../maintainers.nix).eddsteel ];
    platforms = platforms.darwin;
  };
}

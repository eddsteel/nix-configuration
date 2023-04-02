{ pkgs ? import <nixpkgs> {} }:
let
  maintainer = import ../maintainers.nix;
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).circleci_cli."${platform}";
in pkgs.stdenv.mkDerivation rec {
  inherit (versions) version;
  pname = "circleci";
  phases = [ "unpackPhase" "installPhase" ];
  src = pkgs.fetchurl { inherit (versions) name url sha256; };
  installPhase = ''
    mkdir -p $out/bin
    cp circleci $out/bin/
  '';
  meta = with pkgs.lib; {
    description = "Circle CI CLI";
    homepage = "https://circleci.com";
    maintainers = [ maintainer.eddsteel ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}

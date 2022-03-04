{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "spectacle";
  version = "1.2";
  sha = "037kayakprzvs27b50r260lwh2r9479f2pd221qmdv04nkrmnvbn";

  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Spectacle.app "$out/Applications/Spectacle.app"
      '';

  src = pkgs.fetchurl {
    name = "spectacle-${version}.zip";
    url = "https://github.com/eczarny/spectacle/releases/download/${version}/Spectacle+${version}.zip";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Spectacle";
    homepage = "https://www.spectacleapp.com";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

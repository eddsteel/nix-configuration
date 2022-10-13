{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "circleci";
  version = "0.1.21412";
  phases = [ "unpackPhase" "installPhase" ];
  src = pkgs.fetchurl {
    name = "circleci-$version.tar.gz";
    url = "https://github.com/CircleCI-Public/circleci-cli/releases/download/v${version}/circleci-cli_${version}_darwin_amd64.tar.gz";
    sha256 = "111a305qm3rms47jrjv8i556h5zg9f4agibp46xnla68x75j1c6w";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp circleci $out/bin/
  '';

  meta = with pkgs.lib; {
    description = "Circle CI CLI";
    homepage = "https://circleci.com";
    maintainers = [];
    platforms = platforms.linux ++ platforms.darwin;
  };
}

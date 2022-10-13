{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "terraform-docs";
  version = "0.16.0";
  phases = [ "unpackPhase" "installPhase" ];
  src = pkgs.fetchurl {
    name = "terraform-docs-${version}.tar.gz";
    url = "https://terraform-docs.io/dl/v${version}/terraform-docs-v${version}-Darwin-amd64.tar.gz";
    sha256 = "sha256-9IQi4uPEowhSmSF32xfLN7GIEKPoOSQBumnodci7EJA=";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp output/terraform-docs $out/bin/
  '';
  unpackPhase = ''
     mkdir output
     tar -C output -xf $src
  '';

  meta = with pkgs.lib; {
    description = "Terraform Docs";
    homepage = "https://circleci.com";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

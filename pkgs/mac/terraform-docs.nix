{ pkgs ? import <nixpkgs> {} }:
let
  src = (import ../../npins).terraform-docs;
  version = builtins.head (builtins.match ".*/terraform-docs-v(.*)-darwin-arm64.tar.gz" src.url);
in pkgs.stdenv.mkDerivation rec {
  inherit src version;
  pname = "terraform-docs";
  phases = [ "unpackPhase" "installPhase" ];
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
    homepage = "https://terraform-docs.io/";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

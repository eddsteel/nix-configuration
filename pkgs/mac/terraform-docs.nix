{ pkgs ? import <nixpkgs> {} }:
let
  versions = with builtins; (fromJSON (readFile ../versions.json)).terraform_docs;
in pkgs.stdenv.mkDerivation rec {
  inherit (versions) version;
  pname = "terraform-docs";
  phases = [ "unpackPhase" "installPhase" ];
  src = pkgs.fetchurl { inherit (versions) name url sha256; };
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
    maintainers = [ (import ../../maintainers.nix).eddsteel ];
    platforms = platforms.darwin;
  };
}

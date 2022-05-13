{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "wavebox";
  version = "10.101.16.2";
  sha = "sha256-TpSMsj5U6eKZCX2ILOTLtrTpBkvBo+6oLq0D5/p3m/w=";

  buildInputs = [ pkgs.undmg];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Wavebox.app "$out/Applications/Wavebox.app"
      '';

  src = pkgs.fetchurl {
    name = "wavebox-${version}.dmg";
    url = "https://download.wavebox.app/stable/macuniversal/Install%20Wavebox%20${version}.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Wavebox";
    homepage = "https://wavebox.io";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

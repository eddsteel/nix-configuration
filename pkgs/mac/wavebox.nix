{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "wavebox";
  version = "10.99.11.2";
  sha = "1li9qm9wdvzmik3maaqi15wbxnk0i3djhm2gf8flgm1kmdj4kp79";

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

{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "wavebox";
  version = "10.103.39.2";
  sha = "10ss0iap5z9fj4q8w2srqnqhdj9nadxz0pgi10b3ny6pf44lmazb";

  buildInputs = [ pkgs.undmg ];
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

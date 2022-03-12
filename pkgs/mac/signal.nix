{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "signal";
  version = "5.35.0";
  sha = "1bia92fqx131k78ifmvf74p3wpcdwpsxrd3z2hf66znk8w9nwl0q";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Signal.app $out/Applications/Signal.app
  '';

  src = pkgs.fetchurl {
    name = "signal-${version}.dmg";
    url = "https://updates.signal.org/desktop/signal-desktop-mac-universal-${version}.dmg";    
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Signal messenger";
    homepage = "https://signal.org";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

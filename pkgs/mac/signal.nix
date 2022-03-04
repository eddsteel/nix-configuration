{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  pname = "signal";
  version = "5.33.0";
  sha = "1llj30l7pi5zs3zcncb2c8k3ij9rmh84vb3vgywq33rv4s0jjw3r";

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

{ pkgs }:
let
  source = (import ../../npins).firefox;
  version = builtins.head (builtins.match ".*/firefox/releases/([^/]+)/mac/.*" source.url);
in pkgs.stdenv.mkDerivation rec {
  pname = "firefox-mac";
  inherit version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Firefox.app "$out/Applications/Firefox.app"
      '';
  src = pkgs.fetchurl {
    inherit (source) url;
    sha256 = source.hash;
    name = "Firefox-${version}.dmg";
  };

  meta = with pkgs.lib; {
    description = "The Firefox web browser";
    homepage = "https://www.mozilla.org/en-CA/firefox";
    license = licenses.mpl20;
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

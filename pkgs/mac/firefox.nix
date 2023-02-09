{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).firefox;
in pkgs.stdenv.mkDerivation rec {
  pname = "firefox-mac";
  inherit (versions) version;

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Firefox.app "$out/Applications/Firefox.app"
      '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "The Firefox web browser";
    homepage = "https://www.mozilla.org/en-CA/firefox";
    license = licenses.mpl20;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

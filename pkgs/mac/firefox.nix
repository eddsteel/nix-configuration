{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "firefox-mac";
  version = "97.0.1";
  sha = "1xfn139c3dlppinvq905x1rljjgk4h3315fm20jc372gkg0ybb2g";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Firefox.app "$out/Applications/Firefox.app"
      '';

  src = pkgs.fetchurl {
    name = "Firefox-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-CA/Firefox%20${version}.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "The Firefox web browser";
    homepage = "https://www.mozilla.org/en-CA/firefox";
    license = licenses.mpl20;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

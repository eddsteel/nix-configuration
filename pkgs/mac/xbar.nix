{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "xbar";
  version = "2.1.7-beta";
  sha = "0gy73f8gkfa41kvl8rzqbbgrsi9xfz1wkqysw362wkjd1v2afzha";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r xbar.app "$out/Applications/xbar.app"
      '';

  src = pkgs.fetchurl {
    name = "xbar-${version}.dmg";
    url = "https://github.com/matryer/xbar/releases/download/v${version}/xbar.v${version}.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "xbar";
    homepage = "https://xbarapp.com";
    license = licenses.mit;
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

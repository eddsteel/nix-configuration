{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  version = "1.31.3";
  sha = "0rqabavf0gd9359piwpryv6ak21g620ycqk01zdgfliv6kmg6nm0";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Bitwarden.app $out/Applications
  '';

  src = pkgs.fetchurl {
    name = "Bitwarden-${version}.dmg";
    url = "https://github.com/bitwarden/desktop/releases/download/v${version}/Bitwarden-${version}-universal.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Bitwarden password manager";
    homepage = "https://bitwarden.com";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

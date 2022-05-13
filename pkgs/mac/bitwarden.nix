{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  version = "1.32.1";
  sha = "18zrgjxbvn2izqc8ppp6qrcwx0vk94qp88ld88mgiw9y576mbdi0";

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

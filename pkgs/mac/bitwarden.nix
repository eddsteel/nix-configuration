{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  version = "2022.6.1";
  sha = "14dqdgglfi1pg967zkljp5y882im7apwvdnhivq3rcn1vac0fnsl";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Bitwarden.app $out/Applications
  '';

  src = pkgs.fetchurl {
    name = "Bitwarden-${version}.dmg";
    url = "https://github.com/bitwarden/clients/releases/download/desktop-v${version}/Bitwarden-${version}-universal.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Bitwarden password manager";
    homepage = "https://bitwarden.com";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

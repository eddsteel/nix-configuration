{ pkgs }:
pkgs.stdenv.mkDerivation rec {
  pname = "one-password";
  version = "7.9.2";
  sha = "0xz9al9p0xldxq6b52dvkp4bmlh2n6z55xwzcgksilc8aciv5q61";

  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    /usr/sbin/installer -pkg 1Password-${version}.pkg -target ~
    mv ~/Applications/1Password.app $out/Applications
  '';

  src = pkgs.fetchurl {
    name = "1Password-${version}.pkg";
    url = "https://c.1password.com/dist/1P/mac7/1Password-${version}.pkg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "1Password";
    homepage = "https://www.mozilla.org/en-CA/firefox";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

# bailing since installer requires Root install

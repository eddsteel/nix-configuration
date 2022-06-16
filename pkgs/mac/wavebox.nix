{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "wavebox";
  version = "10.102.21.2";
  sha = "1lnlqsxm5vak4jl1fgsc21rn22ls5acl0abn64h0vbdh3sxhfzmp";

  buildInputs = [ pkgs.undmg];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Wavebox.app "$out/Applications/Wavebox.app"
      '';

  src = pkgs.fetchurl {
    name = "wavebox-${version}.dmg";
    url = "https://download.wavebox.app/stable/macuniversal/Install%20Wavebox%20${version}.dmg";
    sha256 = sha;
  };

  meta = with pkgs.lib; {
    description = "Wavebox";
    homepage = "https://wavebox.io";
    maintainers = [];
    platforms = platforms.darwin;
  };
}

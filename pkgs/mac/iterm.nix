{ pkgs ? import <nixpkgs> {} }:
let
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).iterm2;
in pkgs.stdenv.mkDerivation rec {
  pname = "iterm";
  version = versions.version;
  buildInputs = [ pkgs.unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
        mkdir -p "$out/Applications"
        cp -r iTerm.app "$out/Applications/iTerm2.app"
      '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "iTerm 2";
    homepage = "https://iterm2.com";
    license = licenses.gpl2Only;
    maintainers = [];
    platforms = platforms.darwin;
  };
}

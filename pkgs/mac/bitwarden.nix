{ pkgs }:
let
  source = (import ../../npins).bitwarden;
  version = builtins.head (builtins.match ".*Bitwarden-(.*)-universal.dmg&.*" source.url);
  undmgsh = pkgs.writeShellScriptBin "undmg.sh" ''
    # https://discourse.nixos.org/t/help-with-error-only-hfs-file-systems-are-supported-on-ventura/25873/7
    mnt=$(mktemp -d -t ci-XXXXXXXXXX)

    function finish {
      /usr/bin/hdiutil detach $mnt -force
      rm -rf $mnt
    }
    trap finish EXIT

    /usr/bin/hdiutil attach -nobrowse -readonly $src -mountpoint $mnt
    shopt -s extglob
    DEST="$PWD"
    (cd "$mnt"; cp -a !(Applications) "$DEST/")
  '';
in  pkgs.stdenv.mkDerivation rec {
  pname = "bitwarden";
  inherit version;
  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  unpackCmd = "${undmgsh}/bin/undmg.sh";
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Bitwarden.app $out/Applications
  '';

  src = pkgs.fetchurl {
    inherit (source) url;
    name = "Bitwarden-${version}-universal.dmg";
    sha256 = source.hash;
  };

  meta = with pkgs.lib; {
    description = "Bitwarden password manager";
    homepage = "https://bitwarden.com";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

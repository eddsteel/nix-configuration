{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).bitwarden;
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
  inherit (versions) version;
  buildInputs = [ pkgs.undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  unpackCmd = "${undmgsh}/bin/undmg.sh";
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Bitwarden.app $out/Applications
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "Bitwarden password manager";
    homepage = "https://bitwarden.com";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

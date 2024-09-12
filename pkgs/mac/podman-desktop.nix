{ pkgs ? import <nixpkgs> {}}:
let
  versions = (builtins.fromJSON (builtins.readFile ../versions.json)).podman;
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
in
  pkgs.stdenv.mkDerivation rec {
    pname = "podman";
    inherit (versions) version;
    buildInputs = [];
    sourceRoot = ".";
    phases = [ "unpackPhase" "installPhase" ];
    unpackCmd = "${undmgsh}/bin/undmg.sh";
    installPhase = ''
      mkdir -p $out/Applications
      cp -r "Podman Desktop.app" $out/Applications/PodmanDesktop.app
    '';

    src = pkgs.fetchurl { inherit (versions) name url sha256; };

    meta = with pkgs.lib; {
      description = "Podman";
      homepage = "https://podman.dev";
      maintainers = [ maintainers.eddsteel ];
      platforms = platforms.darwin;
    };
  }

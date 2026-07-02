{ pkgs ? import <nixpkgs> {}}:
let
  src = (import ../../npins).podman;
  version = builtins.head (builtins.match "https://github.com/podman-desktop/podman-desktop/releases/download/v(.*)/podman-desktop.*.dmg" src.url);
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
    inherit version src;
    pname = "podman";
    buildInputs = [];
    sourceRoot = ".";
    phases = [ "unpackPhase" "installPhase" ];
    unpackCmd = "${undmgsh}/bin/undmg.sh";
    installPhase = ''
      mkdir -p $out/Applications
      cp -r "Podman Desktop.app" $out/Applications/PodmanDesktop.app
    '';

    meta = with pkgs.lib; {
      description = "Podman";
      homepage = "https://podman.dev";
      maintainers = [ maintainers.eddsteel ];
      platforms = platforms.darwin;
    };
  }

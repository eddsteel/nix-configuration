{ pkgs ? import <nixpkgs> {}}:
let
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).idea;
  idea = pkgs.stdenv.mkDerivation rec {
    pname = "intellij-idea-ce";
    inherit (versions) version;
    buildInputs = [ pkgs.undmg ];
    sourceRoot = ".";
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out/Applications
      cp -r "IntelliJ IDEA CE.app" $out/Applications/IDEA.app
    '';

    src = pkgs.fetchurl { inherit (versions) name url sha256; };

    meta = with pkgs.lib; {
      description = "Intellij IDEA Community Edition";
      homepage = "https://www.jetbrains.com/idea/";
      maintainers = [];
      platforms = platforms.darwin;
    };
  };
in pkgs.writeShellScriptBin "idea" ''
  envchain artifactory ${idea}/Applications/IDEA.app/Contents/MacOS/idea "$@"
''

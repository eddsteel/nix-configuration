{ pkgs ? import <nixpkgs> {}}:
let
  idea = pkgs.stdenv.mkDerivation rec {
    pname = "intellij-idea-ce";
    version = "2022.1.1";
    sha = "sha256-2CT9ns3f7McjgWUm3GI6FZjmM+jzSFEahLm+sVZE0eQ=";

    buildInputs = [ pkgs.undmg ];
    sourceRoot = ".";
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
    mkdir -p $out/Applications
    cp -r "IntelliJ IDEA CE.app" $out/Applications/IDEA.app    
  '';

    src = pkgs.fetchurl {
      name = "intellij-idea-ce-${version}.dmg";
      url = "https://download.jetbrains.com/idea/ideaIC-${version}.dmg";
      sha256 = sha;
    };

    meta = with pkgs.lib; {
      description = "Intellij IDEA Community Edition";
      homepage = "https://www.jetbrains.com/idea/";
      maintainers = [];
      platforms = platforms.darwin;
    };
  }; in
pkgs.writeShellScriptBin "idea" ''
  envchain artifactory ${idea}/Applications/IDEA.app/Contents/MacOS/idea
''


{ pkgs ? import <nixpkgs> {}}:
let
  src = (import ../../npins).idea;
  version = builtins.head (builtins.match ".*/idea-(.+).dmg" src.url);
  meta = with pkgs.lib; {
    inherit version;
    description = "Intellij IDEA Community Edition";
    homepage = "https://www.jetbrains.com/idea/";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
  idea = pkgs.stdenv.mkDerivation rec {
    pname = "intellij-idea-ce";
    inherit src version meta;
    buildInputs = [ pkgs.undmg ];
    sourceRoot = ".";
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out/Applications
      cp -r "IntelliJ IDEA.app" $out/Applications/IDEA.app
    '';
  };
in (pkgs.writeShellApplication  {
  inherit meta;
  runtimeInputs = [pkgs.envchain idea];
  name = "idea";
  text = ''
    envchain gradle ${idea}/Applications/IDEA.app/Contents/MacOS/idea "$@"
  '';
}).overrideAttrs (old: {
  inherit version;
  pname = "idea";
})

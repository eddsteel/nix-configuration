{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib}:
let
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  version = "458";
  java = pkgs.jdk23;
in pkgs.stdenv.mkDerivation rec {
  pname = "trino";
  inherit version;
  src = pkgs.fetchurl {
    name = "trino-cli-${version}-executable.jar";
    url = "https://repo1.maven.org/maven2/io/trino/trino-cli/${version}/trino-cli-${version}-executable.jar";
    sha256 = "sha256-EK8ChR/oyJaexpw3fo2Q0H9BiEJik8jBRQ3dmYiJGEM=";
  };
  phases = [ "installPhase" ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  buildinputs = [ java ];
  dontAutoPatchelf = true;
  installPhase = ''
    mkdir -p $out/bin
    cp ${src} $out/bin/trino
    chmod +x $out/bin/trino
    wrapProgram $out/bin/trino \
        --set JAVA_HOME ${java} \
        --prefix PATH : ${lib.makeBinPath [ java ]}
  '';
  meta = with pkgs.lib; {
    description = "Trino CLI";
    homepage = "https://trino.io/docs/current/client/cli.html";
    maintainers = [ lib.maintainers.eddsteel ];
    license = lib.licenses.asl20;
    platforms = platforms.linux ++ platforms.darwin;
  };
}

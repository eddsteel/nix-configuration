{ pkgs, lib }:
let
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  version = "2024.9.0";
  java = pkgs.jdk22;
in pkgs.stdenv.mkDerivation rec {
  pname = "wvlet";
  inherit version;
  src = pkgs.fetchurl {
    name = "wvlet-cli-${version}.tar.gz";
    url = "https://github.com/wvlet/wvlet/releases/download/v${version}/wvlet-cli-${version}.tar.gz";
    sha256 = "c9b0e651a62b8f9239655d108d7d91c5039a710e7c2affafac24e79ae097f307";
  };
  nativeBuildInputs = [ pkgs.makeWrapper ];
  buildinputs = [ pkgs.jdk22 ];
  installFlags = [ "PREFIX=$(out)" ];
  dontAutoPatchelf = true;
  postFixup = with pkgs; ''
    wrapProgram $out/bin/wv \
      --set JAVA_HOME ${java} \
      --set JAVACMD "${java}/bin/java" \
      --prefix PATH : ${lib.makeBinPath [ java ]}
  '';
  meta = with pkgs.lib; {
    description = "Wvlet";
    homepage = "https://wvlet.org";
    maintainers = [ lib.maintainers.eddsteel ];
    license = lib.licenses.asl20;
    platforms = platforms.linux ++ platforms.darwin;
  };
}

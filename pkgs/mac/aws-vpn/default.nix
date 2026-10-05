{ pkgs }:
let
  src = (import ../../../npins).awsvpn;
  version = "5.4.0";
in pkgs.stdenv.mkDerivation rec {
  pname = "aws-vpn";
  inherit src version;

  nativeBuildInputs = with pkgs; [ cpio xar ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
    xar -xf $src
    cd aws-vpn-client-component.pkg
    zcat < Payload | cpio -i
    cd ..
  '';

  installPhase = ''
    mkdir -p $out/Applications
    cp -R "aws-vpn-client-component.pkg/Applications/AWS VPN Client/AWS VPN Client.app" $out/Applications
 '';

  meta = with pkgs.lib; {
    description = "AWS VPN client";
    homepage = "https://aws.amazon.com/vpn/";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

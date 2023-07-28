{ pkgs }:
let
  versions = (builtins.fromJSON (builtins.readFile ../../versions.json)).awsvpn;
in pkgs.stdenv.mkDerivation rec {
  pname = "aws-vpn";
  inherit (versions) version;

  nativeBuildInputs = with pkgs; [ cpio xar ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
    xar -xf $src
    cd AWS_VPN_Client.pkg
    zcat < Payload | cpio -i
    cd ..
  '';

  installPhase = ''
    mkdir -p $out/Applications
    cp -R "AWS_VPN_Client.pkg/AWS VPN Client/AWS VPN Client.app" $out/Applications
    cp ${./acvc-16.png} "$out/Applications/AWS VPN Client.app/Contents/Resources/acvc-16.png"
  '';

  src = pkgs.fetchurl { inherit (versions) name url sha256; };

  meta = with pkgs.lib; {
    description = "AWS VPN client";
    homepage = "https://aws.amazon.com/vpn/";
    maintainers = [ maintainers.eddsteel ];
    platforms = platforms.darwin;
  };
}

{ pkgs ? import <nixpkgs> {} }:
let
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  versions = (builtins.fromJSON (builtins.readFile ./versions.json)).wavebox."${platform}";
  meta = with pkgs.lib; {
    description = "Wavebox";
    homepage = "https://wavebox.io";
    maintainers = [];
    platforms = platforms.darwin ++ platforms.linux;
  };
  pname = "wavebox";
in if platform == "darwin"
   then pkgs.stdenv.mkDerivation rec {
     inherit (versions) version;
     inherit pname meta;

     buildInputs = [ pkgs.undmg ];
     sourceRoot = ".";
     phases = [ "unpackPhase" "installPhase" ];
     installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Wavebox.app "$out/Applications/Wavebox.app"
      '';

     src = pkgs.fetchurl {
       inherit (versions) name url sha256;
     };
   }
   else let
     desktopItem = pkgs.makeDesktopItem rec {
       name = "Wavebox";
       exec = "wavebox";
       icon = "wavebox";
       desktopName = name;
       genericName = name;
       categories = [ "Network" "WebBrowser" ];
     };
   in with pkgs; stdenv.mkDerivation rec {
     inherit (versions) version;
     inherit pname meta;

     src = pkgs.fetchurl {
       inherit (versions) name url sha256;
     };

     # don't remove runtime deps
     dontPatchELF = true;

     nativeBuildInputs = [ autoPatchelfHook makeWrapper qt5.wrapQtAppsHook ];

     buildInputs = with xorg; [
       libXdmcp
       libXScrnSaver
       libXtst
       libxshmfence
       libXdamage
     ] ++ [
       alsa-lib
       gtk3
       nss
       libdrm
       mesa
       gtk4
       qt5.qtbase
     ];

     runtimeDependencies = [ (lib.getLib udev) libnotify gtk4 ];

     installPhase = ''
    mkdir -p $out/bin $out/opt/wavebox
    cp -r * $out/opt/wavebox

    # provide desktop item and icon
    mkdir -p $out/share/applications $out/share/icons/hicolor/128x128/apps
    ln -s ${desktopItem}/share/applications/* $out/share/applications
    ln -s $out/opt/wavebox/product_logo_128.png $out/share/icons/hicolor/128x128/apps/wavebox.png
  '';

     postFixup = ''
    makeWrapper $out/opt/wavebox/wavebox-launcher $out/bin/wavebox \
    --prefix PATH : ${xdg-utils}/bin
  '';
   }

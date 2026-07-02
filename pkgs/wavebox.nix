{ pkgs ? import <nixpkgs> {}, lib }:
let
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  source = (import ../npins)."wavebox-${platform}";
  version = builtins.head (builtins.match "https://download.wavebox.app/stable/[^/]+/Install%20Wavebox%20(.*)\\.[^.]+$" source.url);
  meta = with pkgs.lib; {
    description = "Wavebox";
    homepage = "https://wavebox.io";
    maintainers = [ lib.maintainers.eddsteel ];
    platforms = platforms.darwin ++ platforms.linux;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    license = licenses.unfree;
  };
  pname = "wavebox";
in if platform == "darwin"
   then pkgs.stdenv.mkDerivation rec {
     inherit version pname meta;

     buildInputs = [ pkgs.undmg ];
     sourceRoot = ".";
     phases = [ "unpackPhase" "installPhase" ];
     installPhase = ''
        mkdir -p "$out/Applications"
        cp -r Wavebox.app "$out/Applications/Wavebox.app"
      '';

     src = pkgs.fetchurl {
       inherit version;
       inherit (source) url;
       sha256 = source.hash;
       name = "wavebox-${version}.dmg";
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
     inherit pname meta version;

     src = pkgs.fetchurl {
       inherit version;
       inherit (source) url;
       sha256 = source.hash;
       name = "wavebox-${version}.tar.gz";
     };

     # don't remove runtime deps
     dontPatchELF = true;
      # ignore optional Qt 6 shim
     autoPatchelfIgnoreMissingDeps = [ "libQt6Widgets.so.6" "libQt6Gui.so.6" "libQt6Core.so.6" ];

     nativeBuildInputs = [ autoPatchelfHook makeWrapper qt5.wrapQtAppsHook copyDesktopItems ];

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
     runHook preInstall
     mkdir -p $out/bin $out/opt/wavebox
     cp -r * $out/opt/wavebox

    # provide desktop icon
    mkdir -p $out/share/icons/hicolor/128x128/apps

    ln -s $out/opt/wavebox/product_logo_128.png $out/share/icons/hicolor/128x128/apps/wavebox.png
    runHook postInstall
  '';

     postFixup = ''
    makeWrapper $out/opt/wavebox/wavebox-launcher $out/bin/wavebox \
    --prefix PATH : ${xdg-utils}/bin
  '';
   }

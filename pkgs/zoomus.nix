{ pkgs ? import <nixpkgs> {}} :
let
  maintainer = import ../maintainers.nix;
  platform = if pkgs.stdenv.isDarwin then "darwin" else "linux";
  allversions = with builtins; fromJSON (readFile ./versions.json);
  versions = allversions.zoom_us."${platform}";
  src = pkgs.fetchurl { inherit (versions) name url sha256; };
  pname = "zoom";
  dontPatchELF = true;
  meta = with pkgs.lib; {
    homepage = "https://zoom.us/";
    description = "zoom.us video conferencing application";
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    license = licenses.unfree;
    platforms = platforms.darwin ++ platforms.linux;
    maintainers = [ maintainer.eddsteel ];
  };
in if platform == "darwin"
   then pkgs.stdenv.mkDerivation rec {
     inherit (versions) version;
     inherit meta pname dontPatchELF src;

     unpackPhase = ''
      xar -xf $src
      zcat < zoomus.pkg/Payload | cpio -i
    '';

     nativeBuildInputs = with pkgs; [
       makeWrapper
       xar
       cpio
     ];

     installPhase = ''
        mkdir -p $out/Applications
        cp -R zoom.us.app $out/Applications/
        runHook postInstall
     '';
   }
   else let
     libs = with pkgs; lib.makeLibraryPath [
       # $ LD_LIBRARY_PATH=$NIX_LD_LIBRARY_PATH:$PWD ldd zoom | grep 'not found'
       alsa-lib
       at-spi2-atk
       at-spi2-core
       atk
       cairo
       cups
       dbus
       expat
       fontconfig
       freetype
       gdk-pixbuf
       glib
       gtk3
       libGL
       libdrm
       libkrb5
       libpulseaudio
       libxkbcommon
       mesa
       nspr
       nss
       pango
       pipewire
       stdenv.cc.cc
       udev
       wayland
       xorg.libX11
       xorg.libXcomposite
       xorg.libXdamage
       xorg.libXext
       xorg.libXfixes
       xorg.libXrandr
       xorg.libXrender
       xorg.libXtst
       xorg.libxcb
       xorg.libxshmfence
       xorg.xcbutilimage
       xorg.xcbutilkeysyms
       xorg.xcbutilrenderutil
       xorg.xcbutilwm
       zlib
     ];
   in pkgs.stdenv.mkDerivation rec {
     inherit (versions) version;
     inherit meta pname dontPatchELF src;

     dontUnpack = true;
     nativeBuildInputs = [ pkgs.makeWrapper ];

     installPhase = ''
        mkdir $out
        tar -C $out -xf $src
        mv $out/usr/* $out/
        runHook postInstall
     '';

     postFixup = ''
        # Desktop File
        substituteInPlace $out/share/applications/Zoom.desktop \
            --replace "Exec=/usr/bin/zoom" "Exec=$out/bin/zoom"

        for i in aomhost zopen zoom ZoomLauncher; do
          patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/opt/zoom/$i
        done

        # ZoomLauncher sets LD_LIBRARY_PATH before execing zoom
        # IPC breaks if the executable name does not end in 'zoom'
        mv $out/opt/zoom/zoom $out/opt/zoom/.zoom
        makeWrapper $out/opt/zoom/.zoom $out/opt/zoom/zoom \
          --prefix LD_LIBRARY_PATH ":" ${libs}

        rm $out/bin/zoom
        # Zoom expects "zopen" executable (needed for web login) to be present in CWD. Or does it expect
        # everybody runs Zoom only after cd to Zoom package directory? Anyway, :facepalm:
        # Clear Qt paths to prevent tripping over "foreign" Qt resources.
        # Clear Qt screen scaling settings to prevent over-scaling.
        makeWrapper $out/opt/zoom/ZoomLauncher $out/bin/zoom \
          --chdir "$out/opt/zoom" \
          --unset QML2_IMPORT_PATH \
          --unset QT_PLUGIN_PATH \
          --unset QT_SCREEN_SCALE_FACTORS \
          --prefix PATH : ${with pkgs; lib.makeBinPath [ coreutils glib.dev pciutils procps util-linux ]} \
          --prefix LD_LIBRARY_PATH ":" ${libs}

        # Backwards compatiblity: we used to call it zoom-us
        ln -s $out/bin/{zoom,zoom-us}
      '';
   }

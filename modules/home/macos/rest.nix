# TODO:
# Create a preferences option that can be contribued to by other modules. Have one activation step that
# imports/writes them all.
#
{config, pkgs, lib, ...}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  cfg = config.macos;
  op = pkgs.writeShellScriptBin "op" ''
    open "$HOME/Applications/Home Manager/$1"
  '';
  op-compl-bsh = pkgs.writeShellScriptBin "op-completion.bash" ''
    _op() {
        local cur prev words cword
        _init_completion || return
       COMPREPLY=($(cd ~/Applications/Home\ Manager && compgen -o dirnames -- "$cur"))
    }

    complete -F _op op
  '';
  op-compl-fsh = pkgs.writeShellScriptBin "op-completion.fish" ''
    complete -p ${op}/bin/op --no-files --exclusive --command op --arguments "(pushd $HOME/Applications/Home\ Manager; __fish_complete_directories; popd)"
 '';
in with lib; {
  options.macos = {
    enable = mkEnableOption "MacOS applications";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      scripts nixUnstable coreutils gnugrep gnused findutils gawk python3
      ps wget gnupg op rectangle karabiner-elements iterm2 swiftdefaultapps
    ] ++ (with mac-apps; [
      intellij-idea-ce wavebox caffeine orbstack
    ]);


    xdg.configFile."karabiner/karabiner.json".source = ../../../files/karabiner.json;

    home.file.".default-gems".text = "bundler";
    home.file.".bundle/config".text = ''
    BUNDLE_BUILD__NOKOGIRI: "--use-system-libraries"
    BUNDLE_DEPLOYMENT: "false"
  '';

    home.file."Library/Preferences/com.knollsoft.Rectangle.plist".source = ../../../files/rectangle-preferences.plist;

    targets.darwin.keybindings = {
      "^u" = "deleteToBeginningOfLine:";
      "^w" = "deleteWordBackward:";
      "~f" = "moveWordForward:";
      "~b" = "moveWordBackward:";
      "~<" = "moveToBeginningOfDocument:";
      "~>" = "moveToEndOfDocument:";
      "~v" = "pageUp:";
      "~d" = "deleteWordForward:";
      "~\010" = "deleteWordBackward:";  /* Option-backspace */
      "~\177" = "deleteWordBackward:";  /* Option-delete */
    };

    # https://github.com/nix-community/home-manager/blob/db00b39a9abec04245486a01b236b8d9734c9ad0/modules/targets/darwin/linkapps.nix
    home.file."Applications/Home Manager".source = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in "${apps}/Applications";

    programs.bash.bashrcExtra = ''
    /usr/bin/ssh-add --apple-use-keychain -q;
    source ${op-compl-bsh}/bin/op-completion.bash
  '';

    programs.fish.shellInit = ''
    source ${op-compl-fsh}/bin/op-completion.fish
  '';

    # standard locations
    home.file."media/film".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Movies";
    home.file."media/music".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Music";
    home.file."txt".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Documents/txt";
    home.file."tmp".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Downloads/tmp";

    home.file."media/desktop.jpg".source = pkgs.fetchurl {
      name = "desktop.jpg";
      url = "https://eddsteel.com/desktop.jpg";
      sha256 = "101mavys8azcah35f1cxiiblrbja5vwrf7kiczgv2byd0ymx9nhz";
    };
    home.file."media/face.jpg".source = pkgs.fetchurl {
      url = "https://eddsteel.com/face.jpg";
      name = "face.jpg";
      sha256 = "1y6hp0n203ccgb2a248xa3i2niflj5wxbd40q69c3p7qd79x3405";
    };

    home.activation."setupMacosHome" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/Documents/txt
    $DRY_RUN_CMD mkdir -p $HOME/Downloads/tmp
    $DRY_RUN_CMD mkdir -p $HOME/media/pictures
  '';

    home.activation."setDefaultApps" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoomus --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    $DRY_RUN_CMD ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoommtg --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    $DRY_RUN_CMD ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoomphonecall --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    $DRY_RUN_CMD ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL callto --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    $DRY_RUN_CMD ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL sip --app "${pkgs.zoom-us}/Applications/zoom.us.app"
  '';

    programs.git.extraConfig = {
      credential.helper = "osxkeychain";
    };
  };
}

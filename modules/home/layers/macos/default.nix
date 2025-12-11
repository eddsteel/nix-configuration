{ config, pkgs, lib, ... }:
let
  cfg = config.layers.macos;
  keychain = ''
    /usr/bin/ssh-add --apple-use-keychain -q;
  '';
  mkOSymlink = config.lib.file.mkOutOfStoreSymlink;
in with lib; {
  options.layers.macos = {
    enable = mkEnableOption "Standard MacOS configuration";
    istat = mkOption {};
    soundsource = mkOption {};
    emacs = mkOption {};
  };

  config = mkIf cfg.enable {
    targets.darwin = {
      dock.enable = true;
      dock.apps = [
        "/System/Applications/System Settings.app"
        "${cfg.emacs}/Applications/Emacs.app"
        "${pkgs.mac-apps.firefox}/Applications/Firefox.app"
        "${pkgs.iterm2}/Applications/iTerm2.app"
        "${pkgs.wavebox}/Applications/Wavebox.app"
        "${pkgs.mac-apps.bitwarden}/Applications/Bitwarden.app"
        "${pkgs.aws-vpn}/Applications/AWS VPN Client.app"
      ];

      defaults = {
        "com.apple.dock" = {
          "autohide" = true;
          "region" = "CA";
          "tilesize" = 75;
          "show-recents" = false;
        };
        "com.apple.menuextra.clock" = {
          "IsAnalog" = true;
        };
        "com.apple.screencapture" = {
          "location" = "~/media/pictures";
        };
        "NSGlobalDomain" = {
          "AppleShowAllExtensions" = true;
        };
        "com.apple.Finder" = {
          "AppleShowAllFiles" = false;
        };

        keybindings = {
          "^u" = "deleteToBeginningOfLine:";
          "^w" = "deleteWordBackward:";
          "~f" = "moveWordForward:";
          "~b" = "moveWordBackward:";
          "~&lt;" = "moveToBeginningOfDocument:";
          "~&gt;" = "moveToEndOfDocument:";
          "~v" = "pageUp:";
          "~d" = "deleteWordForward:";
          "~\010" = "deleteWordBackward:";  /* Option-backspace */
          "~\177" = "deleteWordBackward:";  /* Option-delete */
        };
      };
    };

    # https://github.com/nix-community/home-manager/blob/db00b39a9abec04245486a01b236b8d9734c9ad0/modules/targets/darwin/linkapps.nix
    home.file."Applications/Home Manager".source = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in "${apps}/Applications";

    home.file."media/film".source = mkOSymlink "${config.home.homeDirectory}/Movies";
    home.file."media/music".source = mkOSymlink "${config.home.homeDirectory}/Music";
    home.file."txt".source = mkOSymlink "${config.home.homeDirectory}/Documents/txt";
    home.file."tmp".source = mkOSymlink "${config.home.homeDirectory}/Downloads/tmp";

    home.activation."setupMacosHome" = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD mkdir -p $HOME/Documents/txt
      $DRY_RUN_CMD mkdir -p $HOME/Downloads/tmp
      $DRY_RUN_CMD mkdir -p $HOME/media/pictures
    '';

    home.packages = with pkgs; [
      coreutils gnugrep gnused findutils gawk python3 ps wget less ncurses
    ];

    programs.bash.bashrcExtra = keychain;
    programs.fish.shellInit = keychain;
    programs.git.settings.credential.helper = "osxkeychain";

    programs = {
      istat = {
        enable = true;
        inherit (cfg.istat) email serial;
        extras = builtins.fromJSON (builtins.readFile ./files/istat-extras.json);
      };
      iterm2 = {
        enable = true;
        workingdir = "${config.home.homeDirectory}";
        preferences = import ./files/iterm2-prefs.nix;
      };
      karabiner = {
        enable = true;
        settings-file = ./files/karabiner.json;
      };
      rectangle = {
        enable = true;
        preferences = {
          SUEnableAutomaticChecks = 0;
          SUHasLaunchedBefore = 1;
          hideMenubarIcon = 1;
          launchOnLogin = 1;
          leftHalf = {
            keyCode = 123;
            modifierFlags = 1048576;
          };
          maximize = {
            keyCode = 126;
            modifierFlags = 1048576;
          };
          rightHalf = {
            keyCode = 124;
            modifierFlags = 1048576;
          };
        };
      };
      skhd = {
        enable = true;
        bindings = {
          "shift + alt + cmd + ctrl - e" = "open ${cfg.emacs}/Applications/Emacs.app";
          "shift + alt + cmd + ctrl - f" = "open ${pkgs.mac-apps.firefox}/Applications/Firefox.app";
          "shift + alt + cmd + ctrl - i" = "idea";
          "shift + alt + cmd + ctrl - j" = "if [ -f ~/.nix-profile/bin/jira ]; then ~/.nix-profile/bin/jira; fi";
          "shift + alt + cmd + ctrl - m" = "open ${pkgs.iterm2}/Applications/iTerm2.app";
          "shift + alt + cmd + ctrl - w" = "open ${pkgs.wavebox}/Applications/Wavebox.app";
          "shift + alt + cmd + ctrl - z" = "open ${pkgs.zoom-us}/Applications/zoom.us.app";
          "f7" = "${pkgs.scripts}/bin/emms previous";
          "f8" = "${pkgs.scripts}/bin/emms play-pause";
          "f9" = "${pkgs.scripts}/bin/emms next";
        };
      };
      soundsource = {
        enable = true;
        inherit (cfg.soundsource) name code;
      };
      zoom.enable = true;
    };
  };
}

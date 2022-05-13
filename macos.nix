{config, pkgs, lib, ...}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  home.packages = with pkgs; [
    scripts nixUnstable coreutils gnugrep gnused findutils gawk python3 wget gnupg
    iterm2 xbar spectacle istat-menus skhd intellij-idea-ce wavebox soundsource
  ];

  programs.git.extraConfig.credential.helper = "osxkeychain";

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';

  xdg.configFile."karabiner/karabiner.json".source = ./files/karabiner.json;

  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".text = ''
  #!/usr/bin/env bash
  np=$(${pkgs.scripts}/bin/emms now-playing)
  echo "$np | size=13 length=50"
  '';
  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".executable = true;
  home.file."Library/Application Support/xbar/plugins/youbi.60.sh".source = ./files/xbar-youbi.sh;

  xdg.configFile."skhd/skhdrc".text = ''
  shift + alt + cmd + ctrl - f : open ~/.nix-profile/Applications/firefox.app
  shift + alt + cmd + ctrl - z : open -a zoom.us
  shift + alt + cmd + ctrl - w : open ~/.nix-profile/Applications/Wavebox.app
  shift + alt + cmd + ctrl - e : open ~/.nix-profile/Applications/Emacs.app
  shift + alt + cmd + ctrl - t : open ~/.nix-profile/Applications/iTerm2.app
  shift + alt + cmd + ctrl - j : if [ -f ~/.nix-profile/bin/jira ]; then ~/.nix-profile/bin/jira; fi
  f7 : ${pkgs.scripts}/bin/emms previous
  f8 : ${pkgs.scripts}/bin/emms play-pause
  f9 : ${pkgs.scripts}/bin/emms next
  '';

  home.activation."refreshSKHD" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD launchctl stop org.nixos.skhd
  $DRY_RUN_CMD cp -f ${pkgs.skhd}/Library/LaunchDaemons/org.nixos.skhd.plist ~/Library/LaunchDaemons/
  $DRY_RUN_CMD launchctl unload ~/Library/LaunchDaemons/org.nixos.skhd.plist
  $DRY_RUN_CMD launchctl load ~/Library/LaunchDaemons/org.nixos.skhd.plist
  $DRY_RUN_CMD launchctl start org.nixos.skhd
  '';

  home.file."Library/Application Support/Spectacle/Shortcuts.json".source = ./files/spectacle-shortcuts.json;
  home.file."Library/Preferences/com.divisiblebyzero.Spectacle.plist".source = ./files/com.divisiblebyzero.Spectacle.plist;

  home.file.".default-gems".source = ./files/ruby-default-gems;
  home.file.".bundle/config".source = ./secrets/bundle-config;

  # has licence keys, MAC addresses, GPS etc., so secret
  home.file."Library/Preferences/com.bjango.istatmenus.plist".source = ./secrets/com.bjango.istatmenus.plist;
  home.file."Library/Preferences/com.bjango.istatmenus.status.plist".source = ./secrets/com.bjango.istatmenus.status.plist;
  home.file."Library/Preferences/com.bjango.istatmenus5.extras.plist".source = ./secrets/com.bjango.istatmenus5.extras.plist;
  home.file."Library/Preferences/com.bjango.istatmenus6.extras.plist".source = ./secrets/com.bjango.istatmenus6.extras.plist;

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

  home.activation."copyItermPrefs" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD cp ~/.config/nixpkgs/files/com.googlecode.iterm2.plist ~/Library/Preferences/com.googlecode.iterm2.plist
  '';

  # https://github.com/nix-community/home-manager/blob/db00b39a9abec04245486a01b236b8d9734c9ad0/modules/targets/darwin/linkapps.nix
  home.file."Applications/Home Manager".source = let
    apps = pkgs.buildEnv {
      name = "home-manager-applications";
      paths = config.home.packages;
      pathsToLink = "/Applications";
    };
  in "${apps}/Applications";

  programs.bash.bashrcExtra = "ssh-add --apple-use-keychain -q";

  # standard locations
  home.file."media/film".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Movies";
  home.file."media/photos".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Pictures";
  home.file."media/music".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Music";
  home.file."txt".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Documents/txt";
  home.file."tmp".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/Downloads/tmp";

  home.activation."setupMacosHome" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/Documents/txt
    $DRY_RUN_CMD mkdir -p $HOME/Downloads/tmp
  '';
}

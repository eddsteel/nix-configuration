{config, pkgs, lib, ...}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
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
in {
  home.file.".nix-channels".source = ../../../files/darwin-nix-channels;

  home.packages = with pkgs; [
    scripts nixUnstable coreutils gnugrep gnused findutils gawk python3
    ps wget gnupg colima op rectangle karabiner-elements
    iterm2 skhd swiftdefaultapps
  ] ++ (with mac-apps; [
    xbar istat-menus intellij-idea-ce wavebox soundsource caffeine
  ]);

  xdg.configFile."karabiner/karabiner.json".source = ../../../files/karabiner.json;

  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".text = ''
  #!/usr/bin/env bash
  np=$(${pkgs.scripts}/bin/emms now-playing)
  echo "$np | size=13 length=50"
  '';
  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".executable = true;
  home.file."Library/Application Support/xbar/plugins/youbi.60.sh".source = ../../../files/xbar-youbi.sh;
  home.file."Library/Application Support/xbar/plugins/indoor-temp.60.sh".text = ''
  #!/usr/bin/env bash
  PATH="$HOME/.nix-profile/bin"
  temp=$(nix-shell -p broadlink-cli --command 'broadlink_cli --type 0x5213 --host 192.168.1.162 --mac ec0baeee04b8 --temperature')
  printf "%s°C\n" $temp
  '';
  home.file."Library/Application Support/xbar/plugins/indoor-temp.60.sh".executable = true;

  xdg.configFile."skhd/skhdrc".text = ''
  shift + alt + cmd + ctrl - e : open ~/.nix-profile/Applications/Emacs.app
  shift + alt + cmd + ctrl - f : open ~/.nix-profile/Applications/Firefox.app
  shift + alt + cmd + ctrl - i : idea
  shift + alt + cmd + ctrl - j : if [ -f ~/.nix-profile/bin/jira ]; then ~/.nix-profile/bin/jira; fi
  shift + alt + cmd + ctrl - m : open ~/.nix-profile/Applications/iTerm2.app
  shift + alt + cmd + ctrl - s : open ~/.nix-profile/Applications/Signal.app
  shift + alt + cmd + ctrl - w : open ~/.nix-profile/Applications/Wavebox.app
  shift + alt + cmd + ctrl - z : open ~/.nix-profile/Applications/zoom.us.app
  f7 : ${pkgs.scripts}/bin/emms previous
  f8 : ${pkgs.scripts}/bin/emms play-pause
  f9 : ${pkgs.scripts}/bin/emms next
  '';

  home.file."Library/LaunchAgents/com.koekeishiya.skhd.plist".text = let
      path = builtins.getEnv "PATH";
    in ''
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.koekeishiya.skhd</string>
    <key>ProgramArguments</key>
    <array>
        <string>${pkgs.skhd}/bin/skhd</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>PATH</key>
        <string>${path}</string>
    </dict>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>StandardOutPath</key>
    <string>/tmp/skhd.out</string>
    <key>StandardErrorPath</key>
    <string>/tmp/skhd.err</string>
</dict>
</plist>
  '';

  home.activation."macRefreshSKHD" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD launchctl stop com.koekeishiya.skhd
  $DRY_RUN_CMD launchctl unload ~/Library/LaunchAgents/com.koekeishiya.skhd.plist
  $DRY_RUN_CMD launchctl load -w ~/Library/LaunchAgents/com.koekeishiya.skhd.plist
  $DRY_RUN_CMD launchctl start com.koekeishiya.skhd
  '';

  home.activation."macPrefs" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD defaults write com.apple.menuextra.clock IsAnalog -bool true
    $DRY_RUN_CMD defaults write com.apple.dock "autohide" -bool true
    $DRY_RUN_CMD defaults write com.apple.dock "region" "CA"
    $DRY_RUN_CMD defaults write com.apple.dock "tilesize" "45"
    $DRY_RUN_CMD defaults write com.apple.dock "show-recents" -bool false
    $DRY_RUN_CMD defaults write com.apple.screencapture "location" "~/media/pictures"
    $DRY_RUN_CMD defaults write NSGlobalDomain "AppleShowAllExtensions" -bool "true"
    $DRY_RUN_CMD defaults write com.apple.Finder "AppleShowAllFiles" -bool "false"
    $DRY_RUN_CMD killall Finder

    $DRY_RUN_CMD defaults import com.googlecode.iterm2 ${../../../files}/com.googlecode.iterm2.plist
    $DRY_RUN_CMD defaults import com.rogueamoeba.soundsource ${../../../secrets}/com.rogueamoeba.soundsource.plist
 '';

  home.activation."macDock" = let
    app-def-write = a: ''$DRY_RUN_CMD defaults write com.apple.dock persistent-apps -array-add "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>${a}</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>"'';
    add-dock-app = p: a: app-def-write "${p}/Applications/${a}.app";
  in lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD defaults write com.apple.dock persistent-apps -array
    ${app-def-write "/System/Applications/System Settings.app"}
    ${add-dock-app pkgs.emacs "Emacs"}
    ${add-dock-app pkgs.firefox "Firefox"}
    ${add-dock-app pkgs.iterm2 "iTerm2"}
    ${add-dock-app pkgs.wavebox "Wavebox"}
    ${add-dock-app pkgs.bitwarden "Bitwarden"}
    $DRY_RUN_CMD killall Dock
  '';

  home.file.".default-gems".source = ../../../files/ruby-default-gems;
  home.file.".bundle/config".source = ../../../secrets/bundle-config;

  home.file."Library/Preferences/com.knollsoft.Rectangle.plist".source = ../../../files/rectangle-preferences.plist;
  # has licence keys, MAC addresses, GPS etc., so secret
  home.file."Library/Preferences/com.bjango.istatmenus.plist".source = ../../../secrets/com.bjango.istatmenus.plist;
  home.file."Library/Preferences/com.bjango.istatmenus.status.plist".source = ../../../secrets/com.bjango.istatmenus.status.plist;
  home.file."Library/Preferences/com.bjango.istatmenus5.extras.plist".source = ../../../secrets/com.bjango.istatmenus5.extras.plist;
  home.file."Library/Preferences/com.bjango.istatmenus6.extras.plist".source = ../../../secrets/com.bjango.istatmenus6.extras.plist;

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
}

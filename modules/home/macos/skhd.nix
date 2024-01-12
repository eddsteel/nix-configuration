{config, pkgs, lib, ...}:
let cfg = config.skhd;
in with lib; {
  options.skhd = {
    enable = mkEnableOption "My SKHD configuration";
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.skhd];

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
  };
}

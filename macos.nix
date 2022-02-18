{pkgs, lib, ...}:
{
  home.packages = with pkgs; [scripts nixUnstable coreutils gnugrep gnused findutils gawk python3 skhd];

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
  shift + alt + cmd + ctrl - f : open -a firefox
  shift + alt + cmd + ctrl - z : open -a zoom.us
  shift + alt + cmd + ctrl - w : open -a wavebox
  shift + alt + cmd + ctrl - e : open ~/.nix-profile/Applications/Emacs.app
  shift + alt + cmd + ctrl - j : if [ -f ~/.nix-profile/bin/jira ]; then ~/.nix-profile/bin/jira; fi
  f7 : ${pkgs.scripts}/bin/emms previous
  f8 : ${pkgs.scripts}/bin/emms play-pause
  f9 : ${pkgs.scripts}/bin/emms next
  '';

  home.activation."refreshSKHD" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD ${pkgs.skhd}/bin/skhd -r
  '';

  home.file."Library/Application Support/Spectacle/Shortcuts.json".source = ./files/spectacle-shortcuts.json;
  home.file."Library/Preferences/com.divisiblebyzero.Spectacle.plist".source = ./files/com.divisiblebyzero.Spectacle.plist;

  home.file.".default-gems".source = ./files/ruby-default-gems;
  home.file.".workrc".source = ./secrets/work-bashrc;
  home.file.".bundle/config".source = ./secrets/bundle-config;

  # has licence keys, MAC addresses, GPS etc., so secret
  home.file."Library/Preferences/com.bjango.istatmenus.plist".source = ./secrets/com.bjango.istatmenus.plist;
  home.file."Library/Preferences/com.bjango.istatmenus.status.plist".source = ./secrets/com.bjango.istatmenus.status.plist;
  home.file."Library/Preferences/com.bjango.istatmenus5.extras.plist".source = ./secrets/com.bjango.istatmenus5.extras.plist;
  home.file."Library/Preferences/com.bjango.istatmenus6.extras.plist".source = ./secrets/com.bjango.istatmenus6.extras.plist;

  home.file."Library/KeyBindings/DefaultKeyBinding.dict".source = ./files/DefaultKeyBinding.dict;

  home.activation."copyItermPrefs" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD cp ~/.config/nixpkgs/files/com.googlecode.iterm2.plist ~/Library/Preferences/com.googlecode.iterm2.plist
  '';

}

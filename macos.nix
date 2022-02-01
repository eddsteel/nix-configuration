{pkgs, lib, ...}:

let
  scripts = pkgs.callPackage ../../src/scripts {};
  tg = pkgs.callPackage ../../src/targinator {};
  emms_next = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms next
  '';
  emms_prev = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms previous
  '';
  emms_play = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms play-pause
  '';
in {

  # TODO use local.nix more generically.
  home.packages = with pkgs; [scripts nixUnstable coreutils gnugrep gnused findutils gawk python3];

  programs.git.extraConfig.credential.helper = "osxkeychain";

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';

  home.file.".config/karabiner/karabiner.json".source = ./files/karabiner.json;

  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".text = ''
  #!/usr/bin/env bash
  np=$(${scripts}/bin/emms now-playing)
  echo "$np | size=13 length=50"
  '';
  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".executable = true;
  home.file."Library/Application Support/xbar/plugins/youbi.60.sh".source = ./files/xbar-youbi.sh;

  # apptivate follows symlinks in its configuration, which might be GCed. So we have
  # to write to scripts in its Library dir manually.
  home.activation."copyApptivate" = lib.hm.dag.entryAfter ["writeBoundary"] ''
  $DRY_RUN_CMD echo '${emms_next}' > ~/Library/Application\ Support/Apptivate/emms-next.sh
  $DRY_RUN_CMD echo '${emms_prev}' > ~/Library/Application\ Support/Apptivate/emms-prev.sh
  $DRY_RUN_CMD echo '${emms_play}' > ~/Library/Application\ Support/Apptivate/emms-play.sh
  $DRY_RUN_CMD chmod +x ~/Library/Application\ Support/Apptivate/emms-next.sh
  $DRY_RUN_CMD chmod +x ~/Library/Application\ Support/Apptivate/emms-prev.sh
  $DRY_RUN_CMD chmod +x ~/Library/Application\ Support/Apptivate/emms-play.sh

  if [ -f ~/.nix-profile/bin/jira ]; then
    cat ~/.nix-profile/bin/jira > ~/Library/Application\ Support/Apptivate/jira.sh
    chmod +x ~/Library/Application\ Support/Apptivate/jira.sh
  fi
  '';

  home.file."Library/Application Support/Apptivate/hotkeys".source = ./files/apptivate-hotkeys.plist;

  # plist isn't worth vc
  home.file."Library/Application Support/Spectacle/Shortcuts.json".source = ./files/spectacle-shortcuts.json;
  home.file.".default-gems".source = ./files/ruby-default-gems;

  home.file.".workrc".source = ./secrets/work-bashrc;
}

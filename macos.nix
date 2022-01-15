{pkgs, ...}:

let
  scripts = pkgs.callPackage ../../src/scripts {};
in {

  # TODO use local.nix more generically.
  home.packages = [scripts];

  home.file.".config/karabiner/karabiner.json".source = ./files/karabiner.json;

  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".text = ''
  #!/usr/bin/env bash
  np=$(${scripts}/bin/emms now-playing)
  echo "$np | size=13 length=50"
  '';
  home.file."Library/Application Support/xbar/plugins/emms-show.5.sh".executable = true;
  home.file."Library/Application Support/xbar/plugins/youbi.60.sh".source = ./files/xbar-youbi.sh;

  home.file."Library/Application Support/Apptivate/emms-next.sh".text = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms next
  '';
  home.file."Library/Application Support/Apptivate/emms-next.sh".executable = true;
  home.file."Library/Application Support/Apptivate/emms-prev.sh".text = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms previous
  '';
  home.file."Library/Application Support/Apptivate/emms-prev.sh".executable = true;
  home.file."Library/Application Support/Apptivate/emms-play.sh".text = ''
  #!/usr/bin/env bash
  ${scripts}/bin/emms play-pause
  '';
  home.file."Library/Application Support/Apptivate/emms-play.sh".executable = true;
  home.file."Library/Application Support/Apptivate/hotkeys".source = ./files/apptivate-hotkeys.plist;

  # plist isn't worth vc
  home.file."Library/Application Support/Spectacle/Shortcuts.json".source = ./files/spectacle-shortcuts.json;
  home.file.".default-gems".source = ./files/ruby-default-gems;

  home.file.".workrc".source = ./secrets/work-bashrc;
}

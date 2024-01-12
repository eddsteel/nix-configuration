{config, pkgs, lib, ...}:
let cfg = config.xbar;
in with lib; {
  options.xbar = {
    enable = mkEnableOption "My XBar configuration";
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mac-apps.xbar];

    home.file."Library/Application Support/xbar/plugins/emms-show.5.sh" = {
      text = ''
        #!/usr/bin/env bash
        np=$(${pkgs.scripts}/bin/emms now-playing)
        echo "$np | size=13 length=50"
      '';
      executable = true;
    };
    home.file."Library/Application Support/xbar/plugins/youbi.60.sh" = {
      text = ''
        #!/usr/bin/env bash

        LC_ALL=ja_JP date +"%a曜日"
        echo '---'
        LC_ALL=ja_JP date
      '';
      executable = true;
    };
    home.file."Library/Application Support/xbar/plugins/indoor-temp.60.sh" = {
      text = ''
        #!/usr/bin/env bash
        PATH="$HOME/.nix-profile/bin"
        temp=$(nix-shell -p broadlink-cli --command 'broadlink_cli --type 0x5213 --host 192.168.1.162 --mac ec0baeee04b8 --temperature')
        printf "%s°C\n" $temp
      '';
      executable = true;
    };
  };
}

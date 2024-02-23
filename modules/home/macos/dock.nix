{config, pkgs, lib, ...}:
let cfg = config.targets.darwin.dock;
    type = import ./types.nix { inherit pkgs lib; };
    dock-domain = "com.apple.dock";
    write-def = dom: key: type: value: "$DRY_RUN_CMD defaults write ${dom} ${key} ${type} ${value}";
    app-def-write = a: write-def dock-domain "persistent-apps" "-array-add"
      ''"<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>${a}</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>"'';
    dock-pref-write = p: write-def dock-domain p.key (p.type or "") p.value;
in with lib; {
  options.targets.darwin.dock = {
    enable = mkEnableOption "Manage dock apps and preferences";
    apps = mkOption { type = with types; listOf str;};
  };
  config = mkIf cfg.enable {
    home.activation."macDock" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    ${write-def dock-domain "persistent-apps" "-array" ""}
    ${lib.strings.concatMapStringsSep "\n" app-def-write cfg.apps}
    $DRY_RUN_CMD killall Dock
    '';
  };
}

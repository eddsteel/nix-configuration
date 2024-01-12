{config, pkgs, lib, ...}:
let
  cfg = config.macos.prefs;
  type = import ./types.nix;
  import-prefs = {domain, path}: "$DRY_RUN_CMD defaults import ${domain} ${path}";
in with lib; {
  options.macos.prefs = {
    enable = mkEnableOption "Set MacOS preferences";
    preferences = mkOption {
      type = types.listOf type.preference;
    };
    preferences-files = mkOption {
      type = with types; listOf (attrsOf anything);
    };
  };
  config = mkIf cfg.enable {
    home.activation."macPrefs" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    ${lib.strings.concatMapStringsSep "\n" import-prefs cfg.preferences-files }

    $DRY_RUN_CMD killall Finder
 '';
  };
}

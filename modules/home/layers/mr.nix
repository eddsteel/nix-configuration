{ config, lib, pkgs, ...}:
with lib; let
  cfg = config.mr;
  homedir = config.home.homeDirectory;
  netcheck = "ping -c 1 1.1.1.1 &>/dev/null";
  mkMrConfig = me: repo: let
    name = repo.name;
    pfx = if repo ? pfx then "/${repo.pfx}" else "";
    remote = if repo ? remote then repo.remote else "git@github.com:${me}/${name}";
    stowOpts = "-t ${homedir}${pfx} -d ${homedir}/src --ignore='.gitignore.*'";
    attrs = {
      "checkout" = "git clone ${remote} ${name}";
    } // (optionalAttrs (repo ? stow) {
      "post_checkout" = "stow ${stowOpts} ${name}";
      "post_update" = "stow ${stowOpts} ${name}";
    }) // (optionalAttrs (repo ? fork)) {
      "post_checkout" = "git remote add fork git@github.com:${me}/${name}";
      "post_update" = "git fetch fork";
    };
  in nameValuePair name attrs;
  mrINI = me: repos: lib.generators.toINI {} (listToAttrs (map (mkMrConfig me) repos));
in {
  options = {
    mr = {
      enable = mkEnableOption "mr, a multi-repository management tool";
      repos = mkOption {};
      rootdir = mkOption {};
      github-name = mkOption {};
    };
  };
  config = mkIf cfg.enable {
    home.packages = [ pkgs.mr pkgs.stow pkgs.git ];
    home.file.".mrtrust".text = "${cfg.rootdir}/.mrconfig";
    home.file."${cfg.rootdir}/.mrconfig" = {
      text = mrINI cfg.github-name cfg.repos;
      onChange = ''
        $DRY_RUN_CMD cd ${cfg.rootdir}
        $DRY_RUN_CMD ${netcheck} && mr -j 4 -q up
      '';
    };
  };
}

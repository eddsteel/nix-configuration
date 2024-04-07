{ config, lib, pkgs, ...}:
with lib; let
  cfg = config.mr;
  homedir = config.home.homeDirectory;
  netcheck = "ping -c 1 1.1.1.1 &>/dev/null";
  mrINI = root: me: repos: lib.generators.toINI {} (listToAttrs (map (mkMrConfig root me) repos));
  mkMrConfig = root: me: repo: let
    remote = if repo ? remote then repo.remote else "git@github.com:${me}/${repo.name}";
    stowOpts = "-t ${homedir} -d ${root} --ignore='.gitignore.*'";
    attrs = {
      "checkout" = "git clone ${remote} ${repo.name}";
    } // (optionalAttrs (repo ? stow) {
      "post_checkout" = "stow ${stowOpts} ${repo.name}";
      "post_update" = "stow ${stowOpts} ${repo.name}";
    }) // (optionalAttrs (repo ? fork)) {
      "post_checkout" = "git remote add fork git@github.com:${me}/${repo.name}";
      "post_update" = "git fetch fork";
    };
  in nameValuePair "${root}/${repo.name}" attrs;
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
      text = mrINI cfg.rootdir cfg.github-name cfg.repos;
      onChange = ''
        $DRY_RUN_CMD cd ${cfg.rootdir}
        $DRY_RUN_CMD ${netcheck} && mr -j 4 -q up
      '';
    };
  };
}

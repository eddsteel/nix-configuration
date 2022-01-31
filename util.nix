{ config, lib, ... }:
with lib;
let
  mkMrConfig = repo: let
    name = repo.name;
    pfx = if repo ? pfx then "/${repo.pfx}" else "";
    remote = if repo ? remote then repo.remote else "gh:eddsteel/${name}";
    stowOpts = "-t ${config.home.homeDirectory}${pfx} -d ${config.home.homeDirectory}/src --ignore='.gitignore.*'";
    attrs = {
      "checkout" = "git clone ${remote} ${name}";
    } // (optionalAttrs (repo ? stow) {
      "post_checkout" = "stow ${stowOpts} ${name}";
      "post_update" = "stow ${stowOpts} ${name}";
    });
  in nameValuePair name attrs;
in {
  mrINI = repos: lib.generators.toINI {} (listToAttrs (map mkMrConfig repos));
  emptyIf = pred: lst: if pred then lst else [];
}

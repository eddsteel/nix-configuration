{ config, lib, ... }:
with lib;
let
  stowOpts = "-t ${config.home.homeDirectory} -d ${config.home.homeDirectory}/src --ignore='.gitignore.*'";
  mkMrConfig = repo: let
    name = repo.name;
    remote = if repo ? remote then repo.remote else "gh:eddsteel/${name}";
    attrs = {
      "checkout" = "git clone ${remote} ${name}";
    } // (optionalAttrs (repo ? stow) {
      "post_checkout" = "stow ${stowOpts} ${name}";
      "post_update" = "stow ${stowOpts} ${name}";
    });
  in nameValuePair name attrs;
in {
  mrINI = repos: lib.generators.toINI {} (listToAttrs (map mkMrConfig repos));
}

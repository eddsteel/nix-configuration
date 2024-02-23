{ config, pkgs, lib, ... }:
let
  cfg = config.layers.git;
in with lib; {
  options.layers.git = {
    enable = mkEnableOption "Standard git configuration with configurable email/signing key";
    name = mkOption {};
    email = mkOption {};
    key = mkOption {};
    github-user = mkOption {};
    hub-token = mkOption {};
    emacs = mkOption { default = pkgs.emacs; };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.hub pkgs.git-crypt ];

    xdg.configFile."hub" = {
      text = ''
        github.com:
        - user: ${cfg.github-user}
          oauth_token: ${cfg.hub-token}
          protocol: https
      '';
      executable = true;
    };

    programs.git = {
      enable = true;
      userName = cfg.name;
      userEmail = cfg.email;
      signing.signByDefault = true;
      signing.key = cfg.key;

      aliases = {
        lg = "log --branches --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%aN> %GK? %Creset' --abbrev-commit";
        lgh = "!git lg -10 | head -n 10";
        nuke = "!git branch | while read b; do git branch -d $b; done";
        create = "!hub create";
        pr = "!hub pull-request --browse";
        cpr  = "!f() { git fetch -fu \${2:-origin} refs/pull/$1/head:pr/$1 && git checkout pr/$1; }; f";
        cpr-clean = "!git for-each-ref refs/heads/pr/* --format='%(refname)' | while read ref ; do branch=\${ref#refs/heads/} ; git branch -D $branch ; done";
        init-remote = "!ssh git@eddsteel.com git init --bare ";
        shove = "!git commit -am \"Shoved update $(date)\" && git pull --rebase && git push origin master";
        danglers = "!git fsck --lost-found | grep '^dangling commit' | sed 's/dangling commit //g' | xargs git show -s --oneline";
        decap = "reset HEAD^";
        poke = "commit --allow-empty --amend --reuse-message=HEAD";
        praise = "blame";
        clogp = "-c core.pager='less -p^commit.*$' log -p";
      };

      lfs.enable = true;
      ignores = ["/target/" "lib_managed/" "tags" ".history" ".ensime*"
                 "*~" ".#*" "*#" "*.elc" ".DS_Store" "*.bak" "*.pyc" "/.stack-work/"
                 "/GPATH" "/GRTAGS" "/GTAGS" ".tramp_history" ".agignore"
                 ".rgignore" ".sbt-hydra-history" "ensime.sbt" ".sbt-hydra-history"
                 "/node_modules" ".projectile" "/out/" ".envrc" ".env" ".tool-versions"
                 "/result" "/.idea/" "/tf/"];

      extraConfig = {
        branch.autosetuprebase = "always";
        color.ui = true;
        core.editor = "${cfg.emacs}/bin/emacsclient -s ${config.home.homeDirectory}/run/emacs/server";
        github.user = cfg.github-user;
        init.defaultBranch = "main";
        pull.rebase = true;
        push.default = "current";
        rebase.autostash = true;
        submodule.recurse = true;
        tag.forceSignedAnnotated = true;
        url."git@github.com:".insteadOf = [
          "gh:"
          "https://github.com"
        ];
        url."git@github.com:${cfg.github-user}/".pushInsteadOf = [
          "git://github.com/${cfg.github-user}/"
          "https://github.com/${cfg.github-user}/"
        ];
        # Not using shared machine
        safe.directory = "*";
      };
    };
  };
}

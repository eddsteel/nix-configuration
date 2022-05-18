{ config, pkgs, ... }:
{

  xdg.configFile."hub".source = ./secrets/hub;

  programs.git = {
    userName = "Edd Steel";
    signing.signByDefault = true;

    aliases = {
      lg = "log --branches --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%aN> %GK? %Creset' --abbrev-commit";
      lgh = "!git lg -10 | head -n 10";
      sweep = "!git branch | egrep '^  edd/' | while read b; do git branch -d $b; done";
      mrproper = "!git branch | egrep '^  edd/' | while read b; do git branch -D $b; done";
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
    delta.enable = true;
    ignores = ["/target/" "lib_managed/" "tags" ".history" ".ensime*"
    "*~" ".#*" "*#" "*.elc" ".DS_Store" "*.bak" "*.pyc" "/.stack-work"
    "/GPATH" "/GRTAGS" "/GTAGS" ".tramp_history" ".agignore"
    ".rgignore" ".sbt-hydra-history" "ensime.sbt" ".sbt-hydra-history"
    "/node_modules" ".projectile" "/out/" ".envrc" ".env" ".tool-versions" "/result"];

    extraConfig = {
      branch.autosetuprebase = "always";
      color.ui = true;
      core.editor = "${pkgs.my-emacs}/bin/emacsclient -s ${config.home.homeDirectory}/run/emacs/server";
      github.user = "eddsteel";
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
      url."git@github.com:eddsteel/".pushInsteadOf = [
        "git://github.com/eddsteel/"
        "https://github.com/eddsteel/"
      ];
      # Not using shared machine
      safe.directory = "*";
    };
  };
}

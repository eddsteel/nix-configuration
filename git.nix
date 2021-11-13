{ ... }:
{
  programs.git = {
    userName = "Edd Steel";
    signing.key = "1BE848D76C7C4C51349DDDCC33620159D40385A0";
    signing.signByDefault = true;

    aliases = {
      lg = "log --branches --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%aN> %GK? %Creset' --abbrev-commit";
      lgh = "!git lg -10 | head -n 10";
      sweep = "!git branch | egrep '^  edd/' | while read b; do git branch -d $b; done";
      mrproper = "!git branch | egrep '^  edd/' | while read b; do git branch -D $b; done";
      nuke = "!git branch | while read b; do git branch -d $b; done";
      pr = "!hub pull-request --browse";
      praise = "blame";
      decap = "reset HEAD^";
      cpr  = "!f() { git fetch -fu \${2:-origin} refs/pull/$1/head:pr/$1 && git checkout pr/$1; }; f";
      cpr-clean = "!git for-each-ref refs/heads/pr/* --format='%(refname)' | while read ref ; do branch=\${ref#refs/heads/} ; git branch -D $branch ; done";
      create = "!hub create";
      init-remote = "!ssh git@eddsteel.com git init --bare ";
      shove = "!git commit -am \"Shoved update $(date)\" && git pull --rebase && git push origin master";
      clogp = "-c core.pager='less -p^commit.*$' log -p";
    };

    lfs.enable = true;
    delta.enable = true;
    ignores = ["/target/" "lib_managed/" "tags" ".history" ".ensime*"
    "*~" ".#*" "*#" "*.elc" ".DS_Store" "*.bak" "*.pyc" "/.stack-work"
    "/GPATH" "/GRTAGS" "/GTAGS" ".tramp_history" ".agignore"
    ".rgignore" ".sbt-hydra-history" "ensime.sbt" ".sbt-hydra-history"
    "/node_modules" ".projectile" "/out/"];
    includes = [ { path = ".gitconfig.extra"; } ];

    extraConfig = {
      color.ui = true;
      url."https://github.com/".insteadOf = "gh:";
      url."git@github.com:eddsteel/".pushInsteadOf = [
        "git://github.com/eddsteel/"
        "https://github.com/eddsteel/"
        "gh:eddsteel/"
      ];

      core.editor = "emacsclient";
      push.default = "current";
      branch.autosetuprebase = "always";
      log.mailmap = true;
      tag.forceSignedAnnotated = true;
      pull.rebase = true;
      rebase.autostash = true;
      github.user = "eddsteel";
      submodule.recurse = true;
      init.defaultBranch = "main";
    };
  };
}

{ pkgs, lib, config, ... }:
let
  homedir = config.home.homeDirectory;
  cfg = config.shell;
  aliases = {
    ec = ''${pkgs.emacs}/bin/emacsclient --no-wait --socket=${homedir}/run/emacs/server'';
    ga = ''git add'';
    gam = ''git commit -am'';
    gap = ''git add -p'';
    gau = ''git add -u'';
    gaup = ''git add -up'';
    gb = ''git branch'';
    gc = ''git checkout'';
    gcl = ''git clone'';
    gd = ''git diff'';
    gdc = ''git diff --cached'';
    gf = ''git fetch'';
    gl = ''git pull'';
    glg = ''git lg'';
    glgh = ''git lgh'';
    glr = ''git pull --rebase'';
    gm = ''git commit -m'';
    gma = ''git commit --amend --reuse-message=HEAD'';
    gp = ''git push'';
    gpr = ''hub pull-request'';
    gr = ''git rm'';
    gra = ''git remote add'';
    grr = ''git remote remove'';
    gs = ''git status'';
    gsr = ''find . -type d -name ".git" -print -exec git --git-dir="{}" --work-tree="{}/.." status \;'';
    hms = ''home-manager switch'';
    la = ''ls -a --color=auto'';
    srsly = ''sudo $(fc -ln -1)'';
    stree = ''tree --prune -P *.scala'';

    ".." = ''cd ..'';
    "..." = ''cd ../..'';
    "..3" = ''cd ../../..'';
    "..4" = ''cd ../../../..'';
    "..5" = ''cd ../../../../..'';
    "..6" = ''cd ../../../../../..'';
  };
in with lib; {
  options.shell = {
    enable = mkEnableOption "Use common shell configuration";
    extraAliases = mkOption {
      default = {};
    };
  };
  config = mkIf cfg.enable {
    home.shellAliases = aliases // cfg.extraAliases;
    programs.bash = {
      enable = true;
      historyFile = "${homedir}/.histfile";

      sessionVariables = {
        EDITOR = "${pkgs.emacs}/bin/emacsclient --socket=${homedir}/run/emacs/server";
        ALTERNATE_EDITOR = "${pkgs.emacs}/bin/emacs";
        LESS = " -R ";
        HISTCONTROL = "ignoredups:erasedups";
        HISTSIZE = "100000";
        HISTFILESIZE = "1000000";
        NIXPKGS_CONFIG = toString <nixpkgs-config>;
      };

      bashrcExtra = ''
      if [ -f ${homedir}/.profile ]; then
          . ${homedir}/.profile
      fi

      # i.e. non-nixos that need a bash hook.
      if [ -f /etc/bash.bashrc ]; then
         . /etc/bash.bashrc
         . /etc/bashrc
      fi

      # Keep history up to date across terminals, nix-shells, etc
      shopt -s histappend
      PROMPT_COMMAND="''${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
    '';
    };

    programs.fish = {
      enable = true;
      shellInit = ''
      set -gx EDITOR "${pkgs.emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server"
      set -gx ALTERNATE_EDITOR "${pkgs.emacs}/bin/emacs"
      set NIXPKGS_CONFIG ${toString <nixpkgs-config>}

      # emacs dir tracking
      if [ -n "$INSIDE_EMACS" ]
        function prompt_AnSiT -e fish_prompt
          printf "\eAnSiTh %s\n" (hostname) # this changes more than you would think if using VPNs
          printf "\eAnSiTc %s\n" (pwd)
        end
        printf "\eAnSiTu %s\n" (whoami)
      end
    '';
    };
  };
}

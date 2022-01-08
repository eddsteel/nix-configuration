{ config, pkgs, lib, ... }:
let
  hostname = "draper";
  host = import (./hosts + "/${hostname}.nix") { inherit pkgs config; };
  util = import ./util.nix { inherit config lib; };
  gpgPub = ./files/pubring.gpg;
  gpgSec = ./secrets/secring.gpg;
  netcheck = "ping -c 1 1.1.1.1 2>/dev/null >/dev/null";
in {
  imports = [ ./git.nix ./apps.nix ]
            ++ (if host.gnome then [./gnome.nix] else [])
            ++ (if host.linux then [./linux.nix] else [])
            ++ (if host.macos then [./macos.nix] else [])
  ;

  programs.home-manager.enable = true;
  home.username = host.user;
  home.homeDirectory = host.homeDir;
  home.stateVersion = "21.05";

  home.packages = with pkgs;
    [git nix-prefetch-git mr stow aws]
    ++ host.homePkgs;

  home.file.".face".source = ./files/face;
  home.file.".desktop.jpg".source = ./files/desktop;

  home.file.".mrtrust".text = "${config.home.homeDirectory}/src/.mrconfig";
  home.file."src/.mrconfig".text = util.mrINI host.src.repos;
  home.activation."mrUp" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    cd ~/src/
    ${netcheck} && mr -j 5 up
  '';

  programs.bash = {
    enable = true;
    shellAliases = {
      ec = ''emacsclient --no-wait'';
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
      la = ''ls -a --color=auto'';
      srsly = ''sudo $(fc -ln -1)'';
      ssh = ''TERM=vt100 ssh'';
      stree = ''tree --prune -P *.scala'';
      vlc = ''vlc -f'';
      vm = ''vagrant'';

      ".." = ''cd ..'';
      "..." = ''cd ../..'';
      "..3" = ''cd ../../..'';
      "..4" = ''cd ../../../..'';
      "..5" = ''cd ../../../../..'';
      "..6" = ''cd ../../../../../..'';
    };
    historyFile = "${config.home.homeDirectory}/.histfile";

    sessionVariables = {
      EDITOR = "emacsclient";
      ALTERNATE_EDITOR = "emacs";
      LESS = " -R ";
    };

    bashrcExtra = ''
      if [ -f ${config.home.homeDirectory}/.workrc ]; then
          . ${config.home.homeDirectory}/.workrc
      fi

      # i.e. non-nixos that need a bash hook.
      if [ -f /etc/bash.bashrc ]; then
         . /etc/bash.bashrc
      fi
    '';
  };

  programs.git = {
    enable = true;
    userEmail = host.email;
  }; # the rest is in git.nix

  programs.emacs.enable = true;
  # config is git/mr/stow

  programs.ssh.enable = true;
  home.file.".ssh/id_rsa.pub".source = ./files + "/id_rsa.edd.${hostname}.pub";
  home.file.".ssh/id_rsa".source = ./secrets + "/id_rsa.edd.${hostname}";


  home.file.".aws/credentials".source = ./secrets/aws-credentials;
  ## standard locations
  home.activation."setupMedia" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/media/{music,photos,film}
    $DRY_RUN_CMD mkdir -p $HOME/media/music/{albums,loose}
  '';
  home.activation."setupTxt" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/txt
    cd $HOME/txt
  '';

  programs.gnome-terminal.enable = host.gnome;
  programs.gnome-terminal.themeVariant = "dark";
  programs.gnome-terminal.showMenubar = false;
  programs.gnome-terminal.profile."b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
    visibleName = "Edd";
    audibleBell = false;
    default = true;
    font = "Source Code Pro 12";
    colors = {
      palette = [
        "#363636" "#ff0883" "#83ff08" "#ff8308" "#0883ff" "#8308ff" "#08ff83" "#b6b6b6"
        "#424242" "#ff1e8e" "#8eff1e" "#ff8e1e" "#1e8eff" "#8e1eff" "#1eff8e" "#c2c2c2" ];
      foregroundColor = "#b4e1fd";
      backgroundColor = "#0d1926";
      };
  };

  xdg.userDirs = {
    enable = host.gnome;
    createDirectories = true;
    desktop = "$HOME/tmp";
    documents = "$HOME/txt";
    download = "$HOME/tmp";
    music = "$HOME/media/music";
    templates = "$HOME/media/templates";
    publicShare = "$HOME/share";
    pictures = "$HOME/media/pictures";
    videos = "$HOME/media/videos";
  };

  home.keyboard.options = ["ctrl:nocaps" "compose:rctl"];
  home.keyboard.layout = "ca+eng";

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.jq.enable = true;
  programs.exa.enable = true;
}

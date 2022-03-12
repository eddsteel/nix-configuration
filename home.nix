{ config, pkgs, lib, ... }:
let
  inherit (lib) optional optionals;
  host = import ./host.nix { inherit pkgs config; };
  inherit (import ./util.nix { inherit pkgs config lib host; }) mrINI;
  gpgPub = ./files/pubring.gpg;
  gpgSec = ./secrets/secring.gpg;
  netcheck = "ping -c 1 1.1.1.1 2>/dev/null >/dev/null";
in {
  imports = [ ./git.nix ./apps.nix ]
            ++ optional host.gnome ./gnome.nix
            ++ optional host.linux ./linux.nix
            ++ optional host.macos ./macos.nix;

  programs.home-manager.enable = true;
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "21.05";

  home.packages = with pkgs;
    [git git-secrets nix-prefetch-git mr stow scripts]
    ++ optionals (host ? packages) host.packages
    ++ optionals (host ? scripts) host.scripts;

  home.file.".face".source = ./files/face;
  home.file.".desktop.jpg".source = ./files/desktop;

  home.file.".mrtrust".text = "${config.home.homeDirectory}/src/.mrconfig";
  home.file."src/.mrconfig".text = mrINI host.src.repos;
  home.activation."mrUp" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD cd ~/src/
    $DRY_RUN_CMD ${netcheck} && mr -j 5 up
  '';

  programs.bash = {
    enable = true;
    shellAliases = {
      ec = ''${pkgs.my-emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server'';
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

      ".." = ''cd ..'';
      "..." = ''cd ../..'';
      "..3" = ''cd ../../..'';
      "..4" = ''cd ../../../..'';
      "..5" = ''cd ../../../../..'';
      "..6" = ''cd ../../../../../..'';
    } // host.bashAliases;
    historyFile = "${config.home.homeDirectory}/.histfile";

    sessionVariables = {
      EDITOR = "${pkgs.my-emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server";
      ALTERNATE_EDITOR = "${pkgs.my-emacs}/bin/emacs";
      LESS = " -R ";
    };

    bashrcExtra = ''
      if [ -f ${config.home.homeDirectory}/.profile ]; then
          . ${config.home.homeDirectory}/.profile
      fi

      # i.e. non-nixos that need a bash hook.
      if [ -f /etc/bash.bashrc ]; then
         . /etc/bash.bashrc
         . /etc/bashrc
      fi
    '';
  };

  programs.git = {
    enable = true;
    userEmail = host.email;
    signing.key = host.gpg;
  }; # the rest is in git.nix

  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
  # config is git/mr/stow

  programs.ssh.enable = true;
  home.file.".ssh/id_rsa.pub".source = ./files + "/id_rsa.edd.${host.name}.pub";
  home.file.".ssh/id_rsa".source = ./secrets + "/id_rsa.edd.${host.name}";

  home.file.".aws/credentials".source = ./secrets/aws-credentials;

  ## standard locations
  home.activation."setupMedia" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/media/{music,photos,film}
    $DRY_RUN_CMD mkdir -p $HOME/media/music/{albums,loose}
  '';
  home.activation."setupTxt" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/txt
  '';

  programs.gnome-terminal = {
    enable = host.gnome;
    themeVariant = "dark";
    showMenubar = false;
    profile."b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
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

  home.activation."importKeys" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --import ${gpgPub}
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --import ${gpgSec}
  '';

  programs.firefox = {
    enable = true;    
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      anchors-reveal auto-tab-discard duckduckgo-privacy-essentials bitwarden
    ];
    profiles."default" = {
      id = 0;
      path = "xtqfr4qa.default";
      isDefault = true;
      settings = {
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        "browser.warnOnQuitShortcut" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "services.sync.username" = "edd@eddsteel.com";
        "services.sync.engine.creditcards" = false;
        "services.sync.engine.passwords" = false;
        "accessibility.typeaheadfind.enablesound" = false;
      };
    };
  };
}

{ config, pkgs, lib, ... }:
let
  inherit (lib) optional optionals;

  hostname = builtins.getEnv "HOSTNAME";
  username = builtins.getEnv "USER";
  homedir  = builtins.getEnv "HOME";

  host = import (./hosts + "/${hostname}.nix") { inherit pkgs config; };
  inherit (import ./home/util.nix { inherit pkgs config lib host; }) mrINI;
  gpgPub = ./home/files/pubring.gpg;
  gpgSec = ./home/secrets/secring.gpg;
  netcheck = "ping -c 1 1.1.1.1 2>/dev/null >/dev/null";
in {
  imports = [ ./home/git.nix ./home/apps.nix ./home/emacs.nix ]
            ++ optional host.gnome ./home/gnome.nix
            ++ optional host.linux ./home/linux.nix
            ++ optional host.macos ./home/macos.nix;

  programs.home-manager.enable = true;
  home.username = username;
  home.homeDirectory = homedir;
  home.stateVersion = "21.05";

  home.packages = with pkgs;
    [git git-secrets nix-prefetch-git mr stow scripts]
    ++ optionals (host ? packages) host.packages;

  home.file.".face".source = ./home/files/face;
  home.file.".desktop.jpg".source = ./home/files/desktop;

  home.file.".mrtrust".text = "${config.home.homeDirectory}/src/.mrconfig";
  home.file."src/.mrconfig".text = mrINI host.src.repos;
  home.activation."mrUp" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD cd ~/src/
    $DRY_RUN_CMD ${netcheck} && mr -j 5 up
  '';

  programs.go.enable = host.go;

  programs.bash = {
    enable = true;
    shellAliases = {
      ec = ''${pkgs.emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server'';
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
      hms = ''HOSTNAME="${hostname}" home-manager switch'';
      la = ''ls -a --color=auto'';
      srsly = ''sudo $(fc -ln -1)'';
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
      EDITOR = "${pkgs.emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server";
      ALTERNATE_EDITOR = "${pkgs.emacs}/bin/emacs";
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

  programs.ssh.enable = true;
  home.file.".ssh/id_rsa.pub".source = "${./home/files}/id_rsa.edd.${host.name}.pub";
  home.file.".ssh/id_rsa".source = "${./home/secrets}/id_rsa.edd.${host.name}";

  home.file.".aws/credentials".source = ./home/secrets/aws-credentials;

  home.file.".aspell.conf".text = ''
    data-dir ${config.home.homeDirectory}/.nix-profile/lib/aspell
    lang en_CA
  '';

  ## standard locations
  home.file."media/desktop.jpg".source = ./home/secrets/media/desktop.jpg;
  home.file."media/face.jpg".source = ./home/secrets/media/face.jpg;
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
        "browser.aboutConfig.showWarning" = false;
        "browser.download.dir" = "${config.home.homeDirectory}/tmp";
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        "browser.warnOnQuitShortcut" = false;
        "browser.warnOnCloseOtherTabs" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "services.sync.username" = "edd@eddsteel.com";
        "services.sync.engine.creditcards" = false;
        "services.sync.engine.passwords" = false;
        "services.sync.prefs.sync.browser.tabs.warnOnClose" = false;
        "accessibility.typeaheadfind.enablesound" = false;
        "signon.rememberSignons" = false;
        "signon.autofillForms" = false;
        "signon.generation.enabled" = false;

      };
    };
  };
}

{ config, pkgs, lib, ...}:
with lib;
let
  cfg = config.emacs;
  gradle-mode-fork = epkgs: epkgs.trivialBuild {
    pname = "gradle-mode";
    version = "1";
    src = pkgs.fetchFromGitHub {
      owner = "eddsteel";
      repo = "emacs-gradle-mode";
      rev = "a3b5ea6907e021a7c4177c6cbe4543733da582c7";
      sha256 = "sha256-0jL5QMyzhSKCYHxTvB02HqrgnE9IStDSULiipLXUvW8=";
    };
    buildInputs = [ epkgs.s ];
  };
  ligature = epkgs: epkgs.trivialBuild {
    pname = "ligature";
    version = "1";
    src = pkgs.fetchFromGitHub {
      owner = "mickeynp";
      repo = "ligature.el";
      rev = "9357156a917a021a87b33ee391567a5d8e44794a";
      sha256 = "sha256-Bgb5wFyx0hMilpihxA8cTrRVw71EBOw2DczlM4lSNMs=";
    };
  };
  nano-modeline = epkgs: epkgs.trivialBuild {
    pname = "nano-modeline";
    version = "1";
    src = pkgs.fetchFromGitHub {
      owner = "rougier";
      repo = "nano-modeline";
      rev = "631387df3f6835a975faa1f0c8b44da1fe4b57a9";
      sha256 = "1fmxwxigf25gl0fajcic2h6kza5g74za4l5b4pv5hny7mjkz5kvw";
    };
    patches = [ ./nano-modeline.patch ];
  };
  ob-http-fork = epkgs: epkgs.trivialBuild {
    pname = "ob-http";
    version = "1";
    buildInputs = [ epkgs.s epkgs.cl-lib ];
    src = pkgs.fetchFromGitHub {
      owner = "eddsteel";
      repo = "ob-http";
      rev = "8dc7e46949d1e869662351b120e1e66782ab769a";
      sha256 = "sha256-uEcXgkibKEydCs7WMCFEsOldx7OfjwayPwnIloOs4/Q=";
    };
  };
in {
  options.emacs = {
    enable = mkEnableOption "Emacs";
    package = mkOption { default = pkgs.emacs; };
    local = mkOption {};
  };
  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = cfg.package;
      extraConfig = ''
    (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
    (setq user-emacs-directory (expand-file-name "~/.config/emacs"))
    '';
      extraPackages = epkgs: with epkgs; [
        delight
        basic-theme
        (ligature epkgs)

        consult
        corfu
        embark
        embark-consult
        marginalia
        orderless
        vertico

        anzu
        auto-complete
        auto-highlight-symbol
        browse-at-remote
        dired-collapse
        direnv
        dumb-jump
        evil-numbers
        exec-path-from-shell
        expand-region
        git-gutter
        iedit
        imenu-anywhere
        peep-dired
        quickrun
        rainbow-delimiters
        smartparens
        smooth-scrolling
        string-inflection
        volatile-highlights
        wgrep
        whitespace-cleanup-mode
        wrap-region

        org
        org-beautify-theme
        org-bullets
        org-journal
        ox-gfm
        ob-kotlin
        ob-async
        (ob-http-fork epkgs)
        weather-metno
        pdf-tools

        emms

        lsp-mode
        cargo
        (gradle-mode-fork epkgs)
        sbt-mode
        magit
        rake
        magit-delta
        magit-filenotify
        magit-section
        ripgrep
        graphviz-dot-mode
        hcl-mode
        yaml-mode
        toml-mode
        csv
        ledger-mode
        less-css-mode
        markdown-mode
        nix-mode
        groovy-mode
        haskell-mode
        hi2
        idris-mode
        inf-ruby
        js2-mode
        kotlin-mode
        lua-mode
        python-mode
        restclient-jq
        rjsx-mode
        rust-mode
        scala-mode
        web-mode
        flycheck
        flycheck-kotlin
        flycheck-rust
        flymake-easy
        flymake-go
        flymake-hlint
        (nano-modeline epkgs)
        nano-theme
        restclient
        fish-mode
        multiple-cursors
      ];
    };

    xdg.configFile."emacs/init.el".source = ./init.el;
    xdg.configFile."emacs/edd".source = ./edd;
    xdg.configFile."emacs/local.el".text = cfg.local;
  };
}

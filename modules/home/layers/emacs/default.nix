{ config, pkgs, lib, ...}:
with lib;
let
  cfg = config.layers.emacs;
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
      rev = "04676d57a1e602123a593836745a744d1b2028fb";
      sha256 = "sha256-CoADZ4L4JpoF3JRMb793z2nAq8s1VE3uqb3gqFD7IOA=";
    };
    patches = [ ./nano-modeline.patch ];
  };
  ob-http-fork = epkgs: epkgs.trivialBuild {
    pname = "ob-http";
    version = "1";
    buildInputs = [ epkgs.s epkgs.cl-lib ];
    src = pkgs.fetchFromGitHub {
      owner = "ag91";
      repo = "ob-http";
      rev = "1c6afbdb2e36db1903295ba0c02c9b30f833068d";
      sha256 = "sha256-tZG4WU0QN1pi7tKFYV6GIUa7oaB+CrVaUCxZ/ACkRF8=";
    };
  };
in {
  options.layers.emacs = {
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
        eat
      ];
    };

    xdg.configFile."emacs/init.el".source = ./init.el;
    xdg.configFile."emacs/edd".source = ./edd;
    xdg.configFile."emacs/local.el".text = cfg.local;
  };
}

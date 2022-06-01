{ config, pkgs, ...}:
let
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
  refreshRunningEmacs = ''
    test -f ${config.home.homeDirectory}/run/emacs/server && \
    ${pkgs.my-emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server \
      -e "(load-file user-init-file)"
  '';
  pkgOverrides = (self: super:
    let
      nixpkgsLocal = ../../src/nixpkgs;
    in rec {
      nixpkgs-local = import nixpkgsLocal {};
      magit = self.nixpkgs-local.emacs.pkgs.melpaPackages.magit;
      transient = self.nixpkgs-local.emacs.pkgs.melpaPackages.transient;
    });
in {
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    overrides = pkgOverrides;
    extraPackages = epkgs: with epkgs; [
      delight
      use-package
      nyan-mode
      darkokai-theme
      basic-theme
      (ligature epkgs)

      consult
      corfu
      embark
      embark-consult
      marginalia
      orderless
      vertico
      hydra

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
      ob-http
      weather-metno
      pdf-tools

      emms

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
      restart-emacs
      nano-theme
      nano-modeline
    ];
  };

  xdg.configFile."emacs/init.el" = {
    source = ./files/emacs/init.el;
    onChange = refreshRunningEmacs;
  };

  xdg.configFile."emacs/edd" = {
    source = ./files/emacs/edd;
    onChange = refreshRunningEmacs;
  };

  xdg.configFile."emacs/local.el" = {
    source = ./secrets/local.el;
    onChange = refreshRunningEmacs;
  };
}

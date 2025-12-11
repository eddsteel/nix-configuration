{pkgs, lib, config, ...}:
let
  cfg = config.layers.workstation;
  homedir = config.home.homeDirectory;
  bootstrap-repos = [
    {"name" = "nix-configuration";}
    {"name" = "nix-darwin"; "remote" = "git@github.com:LnL7/nix-darwin";}
    {"name" = "home-manager"; "remote" = "git@github.com:nix-community/home-manager";}
    {"name" = "nixpkgs"; "remote" = "git@github.com:NixOS/nixpkgs"; "fork" = true; }
  ];
  configPath =
    if pkgs.stdenv.hostPlatform.isDarwin then
      "Library/Application Support/amp"
    else
      "${lib.removePrefix config.home.homeDirectory config.xdg.configHome}/amp";

in with lib; {
  imports = [ ./mr.nix ];
  options.layers.workstation = {
    enable = mkEnableOption "Useful apps and configuration for doing work";
    mr-repos = mkOption {};
    github-name = mkOption {};
    gpg-pub = mkOption { default = ./pubring.gpg; };
    gpg-sec = mkOption { default = <nix-config> + "/keys/secring.gpg"; };
    aws-credentials = mkOption {};
    aws-configuration = mkOption {};
    zoomus-config = mkOption { default = ./zoomus.conf; };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gnupg
      git-secrets nix-prefetch-git
      ripgrep unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
      bitwarden-desktop moreutils exfalso wavebox
    ];

    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.jq.enable = true;
    programs.eza.enable = true;
    mr = {
      enable = true;
      github-name = cfg.github-name;
      repos = bootstrap-repos ++ cfg.mr-repos;
      rootdir = "${homedir}/src";
    };

    xdg.configFile."zoomus.conf".source = cfg.zoomus-config;

    home.file.".aws/credentials".text = cfg.aws-credentials;
    home.file.".aws/config-nix".text = cfg.aws-configuration;

    home.file.".aspell.conf".text = ''
      data-dir ${homedir}/.nix-profile/lib/aspell
      lang en_CA
    '';

    home.file."${configPath}/qmk/qmk.ini".text = lib.generators.toINI {} {
      user = {
        keyboard = "kbdfans/kbd75/rev1";
        keymap = "eddsteel";
      };
      find = {
        keymap = "default";
      };
      mass_compile = {
        keymap = "default";
      };
    };

    programs.gradle = {
      enable = true;
      settings = {
        "org.gradle.java.installations.paths" = with pkgs; "${jdk8},${jdk11},${jdk17},${jdk21}";
      };
    };

    home.activation."setupMedia" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/media/{music,photos,film}
    $DRY_RUN_CMD mkdir -p $HOME/media/music/{albums,loose}
  '';
    home.activation."setupTxt" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $HOME/txt
  '';

  home.keyboard.options = ["ctrl:nocaps" "compose:rctl"];
  home.keyboard.layout = "ca+eng";

  home.activation."importKeys" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run --quiet ${pkgs.gnupg}/bin/gpg --quiet $VERBOSE_ARG --import ${cfg.gpg-pub}
    run --quiet ${pkgs.gnupg}/bin/gpg --quiet $VERBOSE_ARG --import ${cfg.gpg-sec}
  '';

  # hack to allow docker to mount current aws config. You suck, docker!
  home.activation."setupAWSConfig" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    rm -f "$HOME/.aws/config"
    cp "$HOME/.aws/config-nix" "$HOME/.aws/config"
  '';
  };
}

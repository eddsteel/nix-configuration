{pkgs, lib, config, ...}:
let
  cfg = config.workstation;
  homedir = config.home.homeDirectory;
  bootstrap-repos = [
    {"name" = "nix-configuration";}
    {"name" = "nix-darwin"; "remote" = "git@github.com:LnL7/nix-darwin";}
    {"name" = "home-manager"; "remote" = "git@github.com:nix-community/home-manager";}
    {"name" = "nixpkgs"; "remote" = "git@github.com:NixOS/nixpkgs"; "fork" = true; }
  ];

in with lib; {
  imports = [ ./mr.nix ];
  options.workstation = {
    enable = mkEnableOption "Useful apps and configuration for doing work";
    mr-repos = mkOption {};
    github-name = mkOption {};
    gpg-pub = mkOption { default = ../../files/pubring.gpg; };
    gpg-sec = mkOption { default = ../../secrets/secring.gpg; };
    aws-credentials = mkOption { default = ../../secrets/aws-credentials; };
    aws-config = mkOption { default = ../../secrets/aws-config; };
    zoomus-config = mkOption { default = ../../files/zoomus.conf; };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      git-secrets nix-prefetch-git jdk17
      duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
      bitwarden signal-desktop moreutils exfalso
    ];

    xdg.configFile."zoomus.conf".source = cfg.zoomus-config;

    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.jq.enable = true;
    programs.exa.enable = true;
    mr = {
      enable = true;
      github-name = cfg.github-name;
      repos = bootstrap-repos ++ cfg.mr-repos;
      rootdir = "${homedir}/src";
    };

    home.file.".aws/credentials".source = cfg.aws-credentials;
    home.file.".aws/config".source = cfg.aws-config;

    home.file.".aspell.conf".text = ''
      data-dir ${homedir}/.nix-profile/lib/aspell
      lang en_CA
    '';
    home.file.".gradle/gradle.properties".text = ''
      org.gradle.java.installations.paths=${pkgs.jdk8},${pkgs.jdk11},${pkgs.jdk17}
    '';

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
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${cfg.gpg-pub}
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${cfg.gpg-sec}
  '';
  };
}

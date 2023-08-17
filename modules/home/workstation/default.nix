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

in with lib; {
  imports = [ ./mr.nix ];
  options.layers.workstation = {
    enable = mkEnableOption "Useful apps and configuration for doing work";
    mr-repos = mkOption {};
    github-name = mkOption {};
    gpg-pub = mkOption { default = ./pubring.gpg; };
    gpg-sec = mkOption { default = <nix-config> + "/keys/secring.gpg"; };
    aws-credentials = mkOption { default = <secrets> + "/aws-credentials"; };
    aws-config = mkOption { default = <secrets> + "/aws-config"; };
    zoomus-config = mkOption { default = ./zoomus.conf; };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gnupg
      git-secrets nix-prefetch-git jdk17
<<<<<<<< HEAD:modules/home/layers/workstation.nix
      duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
      bitwarden moreutils exfalso wavebox
========
      duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain
      tree bitwarden signal-desktop moreutils exfalso
>>>>>>>> 789f169 (re-org people and workstation module):modules/home/workstation/default.nix
    ];

    xdg.configFile."zoomus.conf".source = cfg.zoomus-config;

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

    home.file.".aws/credentials".text = cfg.aws-credentials;
    home.file.".aws/config".text = cfg.aws-configuration;

    home.file.".aspell.conf".text = ''
      data-dir ${homedir}/.nix-profile/lib/aspell
      lang en_CA
    '';

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
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${cfg.gpg-pub}
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${cfg.gpg-sec}
  '';
  };
}

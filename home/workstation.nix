{pkgs, lib, config, ...}:
let
  cfg = config.workstation;
  homedir = config.home.homeDirectory;
  gpgPub = ../files/pubring.gpg;
  gpgSec = ../secrets/secring.gpg;
  repos = [
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
  };
  config = {
    home.packages = with pkgs; [
      git-secrets nix-prefetch-git jdk17
      duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
      bitwarden signal-desktop moreutils exfalso
    ];

    xdg.configFile."zoomus.conf".source = ../files/zoomus.conf;

    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.jq.enable = true;
    programs.exa.enable = true;
    programs.mr = {
      enable = true;
      github-name = cfg.github-name;
      repos = repos ++ cfg.mr-repos;
      rootdir = "${homedir}/src";
    };

    home.file.".aws/credentials".source = ../secrets/aws-credentials;

    home.file.".aspell.conf".text = ''
      data-dir ${homedir}/.nix-profile/lib/aspell
      lang en_CA
    '';
    home.file.".gradle/gradle.properties".text = ''
      org.gradle.java.installations.paths=${pkgs.jdk8},${pkgs.jdk11},${pkgs.jdk17}
    '';
    home.file.".face".source = ../files/face;
    home.file.".desktop.jpg".source = ../files/desktop;
    home.file."media/desktop.jpg".source = ../secrets/media/desktop.jpg;
    home.file."media/face.jpg".source = ../secrets/media/face.jpg;

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
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${gpgPub}
    $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --quiet --import ${gpgSec}
  '';
  };
}

{ config, pkgs, lib, ... }:
let
  work-pkgs = pkgs.callPackages ../../../../src/nix-work {};
  secrets = import ../../home/secrets { inherit work-pkgs; };
in {
  imports = [
    ../../home/emacs.nix
    ../../home/firefox.nix
    ../../home/git.nix
    ../../home/local.nix
    ../../home/macos.nix
    ../../home/shell.nix
    ../../home/ssh.nix
    ../../home/workstation.nix
  ];

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "21.05";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    scripts kotlin gradle terraform terraform-docs hub-local circleci-cli
    docker gettext
  ] ++ work-pkgs.all;

  programs.go.enable = true;

  programs.git = {
    userEmail = secrets.email;
    signing.key = "8433C6F9F807CE8E8DFA99EFB10455BC05772724";
  };

  shell = {
    enable = true;
    extraAliases = {
      "s3" = "AWS_PROFILE=s3-dl-personal ${pkgs.scripts}/bin/s3";
      "gradle" = "envchain gradle gradle";
    } // secrets.aliases;
  };

  workstation = {
    enable = true;
    github-name = "eddsteel";
    mr-repos = [
      {"name" = "git-web-link";}
      {"name" = "scripts";}
    ] ++ secrets.repos;
  };
}

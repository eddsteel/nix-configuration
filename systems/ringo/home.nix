{ config, pkgs, lib, ... }:
let
  work-pkgs = pkgs.callPackages ../../../../src/nix-work {};
  secrets = import ../../secrets { inherit work-pkgs; };
in {
  imports = [ ../../modules/home/macos ../../modules/home ];

  home.stateVersion = "21.05";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    scripts kotlin gradle terraform terraform-docs hub-local circleci-cli
    docker gettext zoom-us aws-vpn dos2unix kubectl
  ] ++ work-pkgs.all;

  programs.go.enable = true;

  local = {
    username = "edd";
    homedir = "/Users/edd";
    hostname = "ringo";
  };

  git = {
    enable = true;
    email = secrets.email;
    key = "8433C6F9F807CE8E8DFA99EFB10455BC05772724";
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

  emacs = {
    enable = true;
  };

  firefox = {
    enable = true;
    sync-user = "edd@eddsteel.com";
  };
}

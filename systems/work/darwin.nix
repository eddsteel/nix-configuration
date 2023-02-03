# Nix-Darwin configuration for my work computer.
{ config, pkgs, ... }:
{
  imports = [ ../per-host.nix ];
  environment.systemPackages = [];

  environment.etc."nix/nix.conf".text = ''
    build-users-group = nixbld
  '';

  perHost = {
    enable = true;
    hostName = "work";
    os = "darwin";
    user = "edd";
    configPath = "/Users/edd/src/nix-configuration";
  };

  environment.pathsToLink = [ "/share/bash-completion" ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.zsh.enable = true;  # default shell on catalina
  programs.fish.enable = true;
  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      fira-code
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
      open-sans
      roboto
      roboto-mono
    ];
  };

  nix = {
    gc.automatic = true;
  };
}

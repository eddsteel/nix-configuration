# Nix-Darwin configuration for my work computer.
{ config, pkgs, ... }:
{
  environment.systemPackages = [];

  environment.etc."nix/nix.conf".text = ''
    build-users-group = nixbld
  '';

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/src/nix-systems/work.nix
  environment.darwinConfig = "$HOME/src/nix-systems/work.nix";
  environment.pathsToLink = [ "/share/bash-completion" ];
  environment.variables = {
    HOSTNAME = "work";
  };

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
      iosevka
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
    gc.dates = "02:00";
  };
}

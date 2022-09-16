{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [];

  environment.etc."nix/nix.conf".text = ''
    build-users-group = nixbld
  '';

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin.nix";

  environment.pathsToLink = [ "/share/bash-completion" ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;
  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      noto-fonts-extra
      noto-fonts-cjk
      fira-code
      iosevka
      open-sans
    ];
  };
}

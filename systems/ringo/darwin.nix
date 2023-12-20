{ config, pkgs, ... }:
let
  secrets = import ../../secrets {};
in {
  imports = [ ../../modules/per-host.nix ];
  environment.systemPackages = [];

  environment.etc."nix/nix.conf".text = ''
    build-users-group = nixbld
  '';

  perHost = {
    enable = true;
    os = "darwin";
  };

  networking.hostName = "ringo";
  launchd.user.envVariables = {
    "EMAIL" = secrets.email;
  };

  environment.pathsToLink = [ "/share/bash-completion" ];
  environment.shells = with pkgs; [ bashInteractive zsh fish ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.zsh.enable = true;  # default shell on catalina
  programs.fish.enable = true;
  programs.fish.shellInit = ''
    # Nix
    if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
      source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
    end
    # End Nix
    '';
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

  # this is currently broken
  documentation = {
    doc.enable = false;
    man.enable = false;
  };
}

{ config, pkgs, ... }:
let
  secrets = builtins.fromTOML (builtins.readFile ./secrets.toml);
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

  environment.pathsToLink = [ "/share/bash-completion" ];
  environment.shells = with pkgs; [ bashInteractive zsh fish ];

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
    packages = with pkgs; [
      fira-code
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      noto-fonts-extra
      open-sans
      roboto
      roboto-mono
      sarasa-gothic
    ];
  };

  nix = {
    enable = true;
    gc.automatic = true;
  };

  # this is currently broken
  documentation = {
    doc.enable = false;
    man.enable = false;
  };
}

{ config, pkgs, ... }:
let
  hostName = "da-shi";
  hosts = import ../hosts.nix;
in {
  imports = [../per-host.nix ./hardware.nix];

  perHost = {
    inherit hostName;
    enable = true;
  };

  networking = {
    inherit hostName;
    inherit (hosts) extraHosts;
  };

  boot.initrd.kernelModules = [ "usb_storage" ];
  boot.kernelParams = [ "nomodeset" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/srv" = {
    device = "/dev/mapper/external";
    options = ["nofail"];
    neededForBoot = false;
  };
  environment.etc."crypttab".text = ''
    external   /dev/sda1   /boot/hdd.key luks,nofail
  '';

  # Set your time zone.
  time.timeZone = "Canada/Pacific";

  i18n.defaultLocale = "en_CA.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.edd = {
     isNormalUser = true;
     extraGroups = [ "wheel" "sudo" ]; # Enable ‘sudo’ for the user.
     packages = [];
     openssh.authorizedKeys.keys = [
      (builtins.readFile ../../files/id_rsa.edd.draper.pub)
      (builtins.readFile ../../files/id_rsa.edd.gusting.pub)
     ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     cryptsetup
     emacs
     vim
     wget
     git
     awscli2
     rsync
     libxfs
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.jellyfin.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 8096 6600];
  networking.firewall.allowedUDPPorts = [ ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  systemd.services.s3sync = {
    wants = [ "network.target" "srv.mount"];
    serviceConfig.Type = "oneshot";
    serviceConfig.User = "edd";
    path = [ pkgs.awscli2 ];
    environment = {
      AWS_SHARED_CREDENTIALS_FILE = "${../../secrets/aws-credentials}";
    };
    script = ''
      aws s3 sync --quiet --size-only /srv/data/ s3://eddsteel-disk/
    '';
  };

  systemd.services.backup = {
    wants = [ "srv.mount" ];
    serviceConfig.Type = "oneshot";
    path = [ pkgs.rsync ];
    script = ''
  D=/srv/backup/da-shi

mkdir -p "$D/current/boot"
mkdir -p "$D/current/etc"
mkdir -p "$D/current/home"

rsync -aHv --size-only --delete /boot "$D/current/"
rsync -aHv --size-only --delete /etc "$D/current/"
rsync -aHv --size-only --delete /home "$D/current/"
    '';
  };

   systemd.timers.backup = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "weekly";
      };
    };

   systemd.timers.s3sync = {
     wantedBy = [ "timers.target" ];
     timerConfig = {
       OnCalendar = "daily";
     };
   };
}

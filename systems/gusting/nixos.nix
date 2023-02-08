{ config, pkgs, ... }:
let
  hostName = "gusting";
  extraHosts = import ../hosts.nix;
  zones = pkgs.callPackage ./zones.nix {};
in {
  imports = [../per-host.nix ./hardware.nix];

  perHost = {
    inherit hostName;
    enable = true;
  };

  networking = {
    inherit hostName extraHosts;
    firewall.allowedTCPPorts = [ 22 53 8096 ];
    firewall.allowedUDPPorts = [ 53 ];
  };

  boot.initrd.kernelModules = [ "usb_storage" ];
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

#  fileSystems."/".options = ["noatime"];
#  fileSystems."/srv" = {
#    device = "/dev/mapper/external";
#    options = ["nofail"];
#    neededForBoot = false;
#  };
#  environment.etc."crypttab".text = ''
#    external   /dev/sda1   /boot/hdd.key luks
#  '';

  # Set your time zone.
  time.timeZone = "Canada/Pacific";

  i18n.defaultLocale = "en_CA.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  users.users.edd = {
     isNormalUser = true;
     extraGroups = [ "wheel" ];
     packages = [];
     openssh.authorizedKeys.keys = [ (builtins.readFile ../../files/id_rsa.edd.draper.pub) ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     cryptsetup
     emacs
     vim
     wget
     git
     libraspberrypi
     awscli2
     rsync
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

#  services.jellyfin.enable = true;

  services.unbound = {
    enable = true;
    settings = {
      include = "${zones}/blocklist.conf";
      server = {
        interface = ["0.0.0.0"];
        access-control = ["127.0.0.0/24 allow" "192.168.1.0/24 allow"];
      };
      forward-zone = [
        {
          name = ".";
          forward-addr = ["1.0.0.1@853#cloudflaredns.com" "1.1.1.1@853#cloudflaredns.com"];
        }
      ];
    };
  };
  
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
    wants = [ "network.target" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.User = "edd";
    path = [ pkgs.awscli2 ];
    script = ''
      aws s3 sync --size-only --quiet /srv/data/ s3://eddsteel-disk/
    '';
  };

  systemd.services.backup = {
    wants = [ "srv.mount" ];
    serviceConfig.Type = "oneshot";
    path = [ pkgs.rsync ];
    script = ''
D=/srv/backup/gusting

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

  nix.distributedBuilds = true;
  nix.buildMachines = [ {
    hostName = "draper";
    sshUser = "builder";
    systems = [ "x86_64-linux" "aarch64-linux" ];
    supportedFeatures = [ "big-parallel" ]; # allow big compile tasks
  } ];
}

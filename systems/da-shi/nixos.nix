{ config, pkgs, lib, ... }:
let
  sops-nix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
  };
  hostName = "da-shi";
  hosts = import ../hosts.nix { inherit lib; };
  secrets = import ../../secrets;
in {
  imports = [
    ../../modules/per-host.nix
    ./hardware.nix
    "${sops-nix}/modules/sops"
  ];

  perHost = {
    enable = true;
    inherit hostName;
  };

  networking = {
    hostName = "da-shi";
    inherit (hosts) extraHosts;
    firewall.allowedTCPPorts = [ 22 4000 8096 8200 6600];
    firewall.allowedUDPPorts = [ 1900 ];
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

  sops.defaultSopsFile = ../../sops/secrets.yaml;
  sops.secrets."pleroma/secrets.exs".owner = config.users.users.pleroma.name;
  sops.secrets."backup/env".owner = config.users.users.edd.name;

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
    exiftool
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
  services.minidlna = {
    enable = true;
    settings.media_dir = [ "V,/srv/data/film" "V,/srv/data/television" "A,/srv/data/albums" ];
    openFirewall = true;
  };
  services.pleroma = {
    enable = true;
    configs = [ (lib.fileContents ../../files/pleroma-config.exs) ];
    secretConfigFile = "/run/secrets/pleroma/secrets.exs";
  };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
    dataDir = "/srv/data/base";
    initialScript = pkgs.writeText "pleroma-initScript" ''
      CREATE ROLE pleroma WITH LOGIN PASSWORD '${secrets.database.pleroma.password}' CREATEDB;
      CREATE DATABASE pleroma;
      GRANT ALL PRIVILEGES ON DATABASE pleroma TO pleroma;
      ALTER DATABASE pleroma OWNER to pleroma;
    '';
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
    wants = [ "network.target" "srv.mount"];
    serviceConfig.Type = "oneshot";
    serviceConfig.User = "edd";
    serviceConfig.EnvironmentFile = /run/secrets/backup/env;
    path = [ pkgs.awscli2 ];
    script = ''
      aws s3 sync --quiet --size-only /srv/data/ s3://eddsteel-disk/
    '';
  };

  systemd.timers.s3sync = {
     wantedBy = [ "timers.target" ];
     timerConfig = {
       OnCalendar = "daily";
     };
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
}

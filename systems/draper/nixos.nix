{ config, pkgs, lib, ... }:
let
  sops-nix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
  };
  hosts = import ../hosts.nix { inherit lib; };
  people = import ../people.nix { inherit lib; };
  secrets = builtins.fromTOML (builtins.readFile ./secrets.toml);
in {
  imports = [
    ../../modules/per-host.nix
    ./hardware.nix
    "${sops-nix}/modules/sops"
  ];

  perHost = {
    enable = true;
  };

  networking = {
    hostName = "draper";
    inherit (hosts) extraHosts;
    firewall.allowedTCPPorts = [
      22 4242 8000 8096 8200 8300 8301 8302 8500 8543
    ];
    firewall.allowedUDPPorts = [ 1900 ];
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.luks.devices = {
      "user" = {
        device = "/dev/disk/by-uuid/0e8169a6-a23d-4651-9042-3b5d08f2679e";
      };
    };
  };

  fileSystems = {
    "/".options = ["noatime"];
    "/boot".options = ["noatime"];
    "/home".options = ["noatime"];
    "/nix".options = ["noatime"];
    "/mnt/nfs/film" = {
      device = "da-shi:/film";
      fsType = "nfs";
      options = ["noatime" "noauto" "x-systemd.automount" "x-systemd.idle-timeout=3600"];
    };
    "/mnt/nfs/books" = {
      device = "da-shi:/books";
      fsType = "nfs";
      options = ["noatime" "noauto" "x-systemd.automount" "x-systemd.idle-timeout=3600"];
    };
  };
  # for NFS
  services.rpcbind.enable = true;

  # Set your time zone.
  time.timeZone = "Canada/Pacific";
  # time.timeZone = "Europe/London";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  sops.defaultSopsFile = ../../sops/secrets.yaml;
  sops.secrets."consul/ca.pem".owner = "consul";
  sops.secrets."consul/server.crt".owner = config.users.users.consul.name;
  sops.secrets."consul/server.key".owner = config.users.users.consul.name;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;

  i18n.inputMethod.enabled = "ibus";
  i18n.inputMethod.ibus.engines = with pkgs.ibus-engines; [
    anthy hangul libpinyin m17n mozc table table-others
  ];

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.cnijfilter2 ];

  # Enable sound.
  sound.enable = false;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.users.edd = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" "docker" "cdrom" "disk"];
    openssh.authorizedKeys.keys = people.pubkeys;
  };

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  users.users.builder = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      (people.pubkey "root" "gusting")
    ];
  };
  nix.settings.trusted-users = [ "builder" ];

  environment.shells = [ pkgs.fish ];

  environment.systemPackages = with pkgs; [
    awscli2
    git
    links2
    mr
    nixos-option
    playerctl
    vim
    wayland
    wget
  ];

  fonts = {
      fontDir.enable = true;
      packages = with pkgs; [
        corefonts
        fira-code
        inconsolata
        terminus_font
        dejavu_fonts
        font-awesome
        ubuntu_font_family
        source-code-pro
        source-sans-pro
        source-serif-pro
        ipafont
        kochi-substitute
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        open-sans
      ];
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
  };

  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.keybase.enable = true;
  services.consul = {
    enable = true;
    interface.bind = "wlp0s20f3";
    interface.advertise = "wlp0s20f3";
    extraConfig = {
      ui_config.enabled = true;
      retry_join = ["gusting"];
      datacenter = "edd";
      ports = { https = 8543;};
      ca_file = "/run/secrets/consul/ca.pem";
      cert_file = "/run/secrets/consul/server.crt";
      key_file = "/run/secrets/consul/server.key";
      http_config = {
        response_headers = {
          Access-Control-Allow-Origin = "*";
          Access-Control-Allow-Methods = "GET,PUT,POST,DELETE";
          Access-Control-Allow-Headers = "content-type,user-agent";
        };
      };
    };
  };

  environment.etc."consul.d/brainzo.json".text = ''{
    "service": {
      "name": "brainzo",
      "tags": [],
      "port": 4242,
      "checks": [
        {
          "id": "api",
          "name": "You OK bud?",
          "http": "http://localhost:4242/bleep",
          "method": "GET",
          "interval": "30s",
          "timeout": "50ms"
        }
      ]
    }
  }'';

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTRS{idVendor}=="0bda", ATTRS{idProduct}=="5401", ATTR{power/wakeup}="enabled", ATTR{driver/3-7.4/power/wakeup}="enabled"
  '';

  services.minidlna = {
    enable = true;
    settings.media_dir = ["V,/home/media/film" "A,/home/media/music/albums"];
  };

  security.pki.certificates = [ secrets.consul.server-crt ];

  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  nix = {
    gc.automatic = true;
    gc.dates = "02:00";
  };

  virtualisation.docker.enable = true;
  services.jellyfin.enable = true;
}

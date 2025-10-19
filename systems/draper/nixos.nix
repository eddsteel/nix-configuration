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
      22 4242 8000 8096 8200 8300 8301 8302 8384 8500 8543 22000
    ];
    firewall.allowedUDPPorts = [ 1900 22000 21027];
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
#    "/mnt/nfs/film" = {
#      device = "da-shi:/film";
#      fsType = "nfs";
#      options = ["noatime" "noauto" "x-systemd.automount" "x-systemd.idle-timeout=3600"];
#    };
    "/mnt/nfs/book" = {
      device = "da-shi:/book";
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.desktopManager.gnome.enable = true;
  services.displayManager.gdm.enable = true;

  i18n.inputMethod =  {
    enable = true;
    type = "ibus";
    ibus.engines = with pkgs.ibus-engines; [
      anthy libpinyin m17n mozc table table-others
    ];
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.cnijfilter2 ];

  # Enable sound.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer
    ];
  };

  hardware.keyboard.qmk.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.users.edd = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" "docker" "cdrom" "disk" "adbusers"];
    openssh.authorizedKeys.keys = people.pubkeys;
  };

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
        noto-fonts-cjk-sans
        noto-fonts-emoji
        open-sans
        sarasa-gothic
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

  programs.adb.enable = true;

  programs.fish.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.keybase.enable = true;
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTRS{idVendor}=="0bda", ATTRS{idProduct}=="5401", ATTR{power/wakeup}="enabled", ATTR{driver/3-7.4/power/wakeup}="enabled"
  '';

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
  services.jellyfin.enable = false;
  services.syncthing = with secrets.syncthing; {
    enable = true;
    user = "edd";
    dataDir = "/home/edd/media";
    configDir = "/home/edd/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true;
    settings = {
      gui = { inherit user password; };

      devices = {
        "ereader" = { id = ereader-id;};
        "phone"   = { id = phone-id;};
      };

      folders = {
        # ~/Books
        "Books" = {
          path = "~/media/books";
          devices = [ "ereader" "phone" ];
        };
        # ~/DCIM
        "Phone Photos" = {
          path = "~/media/photos/phone";
          devices = [ "phone" ];
        };
        #
        "Reader Documents" = {
          path = "~/media/reader";
          devices = [ "ereader" ];
        };
        # ~/Sync
        "Phone Sync" = {
          path = "~/media/phone";
          devices = [ "phone" ];
        };
      };
    };
  };
}

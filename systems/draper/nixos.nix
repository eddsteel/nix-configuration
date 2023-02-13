{ config, pkgs, ... }:
let
  hostName = "draper";
  hosts = import ../hosts.nix;
  consul-cert = /nix/store/pz1jqbq4ja3ms2cvbbmjlkc3k85klcm8-consul-cert;
in {
  imports = [ ../per-host.nix ./hardware.nix ];

  perHost = {
    enable = true;
    inherit hostName;
  };

  networking = {
    inherit hostName;
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
  };

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
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.users.edd = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" "docker" "cdrom"];
    openssh.authorizedKeys.keys = [
      (builtins.readFile ../../files/id_rsa.edd.work.pub)
      (builtins.readFile ../../files/id_rsa.edd.gusting.pub)
    ];
  };

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  users.users.builder = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      (builtins.readFile ../../files/id_rsa.root.gusting.pub)
    ];
  };
  nix.settings.trusted-users = [ "builder" ];

  environment.shells = [ pkgs.fish ];

  environment.systemPackages = with pkgs; [
    awscli2
    git
    home-manager
    links2
    mr
    nixos-option
    playerctl
    vim
    wget
  ];

  fonts = {
      fontDir.enable = true;
      fonts = with pkgs; [
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

  programs.steam.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.keybase.enable = true;
  services.consul = {
    enable = true;
    interface.bind = "enp0s13f0u3";
    interface.advertise = "enp0s13f0u3";
    extraConfig = {
      ui_config.enabled = true;
      retry_join = ["192.168.1.39"];
      datacenter = "edd";
      ports = { https = 8543;};
      ca_file = "${consul-cert}/ca.pem";
      cert_file = "${consul-cert}/server.crt";
      key_file = "${consul-cert}/server.key";
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

  security.pki.certificates = [ "${consul-cert}/server.crt"];

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
#  hardware.opengl = {
#    enable = true;
#    extraPackages = with pkgs; [
#      intel-media-driver
#      vaapiIntel
#      vaapiVdpau
#      libvdpau-va-gl
#      intel-compute-runtime
#    ];
#  };

  services.jellyfin.enable = true;
}

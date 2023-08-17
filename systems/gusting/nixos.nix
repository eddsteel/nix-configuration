{ config, pkgs, lib, ... }:
let
  hostName = "gusting";
  zones = pkgs.callPackage ./zones.nix {};
  sops-nix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
  };
  hosts = import ../hosts.nix { inherit lib; };
  people = import ../../people {inherit lib; };
  virtualHost = svc: {
    name = "${svc.name}.${hosts.domain}";
    value = {
      enableACME = true;
      acmeRoot = null;
      forceSSL = true;
      locations."/" = {
        proxyPass = svc.url;
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };
in {
  imports = [
    ../../modules/per-host.nix
    ./hardware.nix
    "${sops-nix}/modules/sops"
  ];

  perHost = {
    inherit hostName;
    enable = true;
  };

  networking = {
    inherit (hosts) extraHosts;
    firewall.allowedTCPPorts = [ 22 80 443 8300 8301 8302 8543 8500 ];
    firewall.allowedUDPPorts = [ 53 8300 8301 8302 ];
  };

  boot.initrd.kernelModules = [ "usb_storage" ];
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  fileSystems."/".options = ["noatime"];
  fileSystems."/srv" = {
    device = "/dev/mapper/external";
    options = ["nofail"];
    neededForBoot = false;
  };
  environment.etc."crypttab".text = ''
    external   /dev/sda1   /boot/hdd.key luks
  '';

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
     openssh.authorizedKeys.keys = people.pubkeys;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     cryptsetup
     dig
     emacs
     vim
     wget
     git
     libraspberrypi
     awscli2
     rsync
     r53-ddns
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

  sops.defaultSopsFile = ../../sops/secrets.yaml;
  sops.secrets."consul/ca.pem".owner = config.users.users.consul.name;
  sops.secrets."consul/server.crt".owner = config.users.users.consul.name;
  sops.secrets."consul/server.key".owner = config.users.users.consul.name;
  sops.secrets."route53/env" = {};

  services.openssh.enable = true;
  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = ["0.0.0.0" "::0"];
        access-control = ["127.0.0.0/24 allow" "192.168.1.0/24 allow"];
        domain-insecure = "${hosts.domain}.local";
        local-zone = [
          ''"localhost." static''
          ''"127.in-addr.arpa." static''
          ''"${hosts.domain}" transparent''
        ];
        # TODO refactor with hosts vars
        local-data = [
        ''"localhost. 10800 IN NS localhost."''
        ''"localhost. 10800 IN SOA localhost. nobody.invalid. 1 3600 1200 604800 10800"''
        ''"localhost. 10800 IN A 127.0.0.1"''
        ''"127.in-addr.arpa. 10800 IN NS localhost."''
        ''"127.in-addr.arpa. 10800 IN SOA localhost. nobody.invalid. 2 3600 1200 604800 10800"''
        ''"1.0.0.127.in-addr.arpa. 10800 IN PTR localhost."''
        ''"da-shi.${hosts.domain}  IN  A  ${hosts.hosts.da-shi.ip4}"''
        ''"draper.${hosts.domain}  IN  A  ${hosts.hosts.draper.ip4}"''
        ''"blinds.${hosts.domain}  IN  A  ${hosts.hosts.blinds.ip4}"''
        ''"gusting.${hosts.domain}  IN  A  ${hosts.hosts.gusting.ip4}"''
        ] ++ (map
          (s: ''"${s.name}.${hosts.domain}  IN  A  ${hosts.hosts.gusting.ip4}"'')
          hosts.services);
        private-domain = [ '' "${hosts.domain}."''];
      };
      forward-zone = {
          name = ".";
          forward-tls-upstream = "yes";
          forward-addr = ["1.0.0.1@853#cloudflare-dns.com" "1.1.1.1@853#cloudflare-dns.com"];
        };
      include = "${zones}/blocklist.conf";
    };
  };
  services.consul = {
    enable = true;
    interface.bind = "eth0";
    interface.advertise = "eth0";
    extraConfig = {
      ui_config.enabled = true;
      datacenter = "edd";
      ports = { https = 8543;};
      bootstrap = true;
      bootstrap_expect = 1;
      server = true;
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
  security.pki.certificates = [ "../../secrets/consul-server.crt" ];

  security.acme = {
    acceptTerms = true;
    defaults = {
      email = "edd@eddsteel.com";
      dnsProvider = "route53";
      credentialsFile = /run/secrets/route53/env;
    };
  };

  users.users.nginx.extraGroups = [ "acme" ];

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    # other Nginx options;
    virtualHosts = with builtins; listToAttrs (map virtualHost hosts.services);
  };

  system.copySystemConfiguration = true;
  system.stateVersion = "22.11";

  systemd.services.r53-ddns = {
    wants = [ "network.target"];
    serviceConfig.EnvironmentFile = /run/secrets/route53/env;
    serviceConfig.Type = "oneshot";
    path = [ pkgs.r53-ddns ];
    script = with builtins; lib.strings.concatStringsSep "\n" (map (svc: ''
      r53-ddns -hostname "${svc.name}" -domain "${hosts.domain}" -zone-id $AWS_HOSTED_ZONE_ID
    '') hosts.services);
  };

  systemd.timers.r53-ddns = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "weekly";
    };
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

  nix.distributedBuilds = true;
  nix.buildMachines = [ {
    hostName = "draper";
    sshUser = "builder";
    systems = [ "x86_64-linux" "aarch64-linux" ];
    supportedFeatures = [ "big-parallel" ]; # allow big compile tasks
  } ];
}

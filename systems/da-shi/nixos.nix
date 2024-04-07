{ config, pkgs, lib, ... }:
let
  sops-nix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
  };
  hostName = "da-shi";
  hosts = import ../hosts.nix { inherit lib; };
  people = import ../people.nix { inherit lib; };
  secrets = import <secrets>;
  zones = pkgs.callPackage ./zones.nix {};
  srvBindMount = dir: {
    device = "/srv/${dir}";
    options = [ "bind" "x-systemd.requires-mounts-for=/srv" "nofail"];
  };
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

  perHost.enable = true;

  networking = {
    hostName = "da-shi";
    inherit (hosts) extraHosts;
    firewall.allowedTCPPorts = [ 22 80 443 2049 3000 4000 8300 8301 8302 8543 8500 8096 8200 6600 8080];
    firewall.allowedUDPPorts = [ 1900 53 55 8300 8301 8302];
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

  fileSystems."/export/film" = srvBindMount "data/film2";
#  fileSystems."/export/albums" = srvBindMount "data/albums";
#  fileSystems."/export/television" = srvBindMount "data/television";
  fileSystems."/export/books" = srvBindMount "data/books";

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
  sops.secrets."consul/ca.pem".owner = config.users.users.consul.name;
  sops.secrets."consul/server.crt".owner = config.users.users.consul.name;
  sops.secrets."consul/server.key".owner = config.users.users.consul.name;
  sops.secrets."route53/env" = {};
  sops.secrets."backup/env".owner = config.users.users.edd.name;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.edd = {
     isNormalUser = true;
     extraGroups = [ "wheel" "sudo" ]; # Enable ‘sudo’ for the user.
     packages = [];
     openssh.authorizedKeys.keys = people.pubkeys;
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

    dig
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
  services.openssh.enable = true;
  services.jellyfin.enable = true;
  services.minidlna = {
    enable = true;
    settings.media_dir = [ "V,/srv/data/film" "V,/srv/data/television" "A,/srv/data/albums" ];
    openFirewall = true;
  };
  services.grafana = {
    enable = true;
    dataDir = "/srv/data/grafana";
    settings = {
      server = {
        # Listening Address
        http_addr = "0.0.0.0";
        # and Port
        http_port = 3000;
        # Grafana needs to know on which domain and URL it's running
        domain = "stats.moron.city";
      };

      "plugin.marcusolsson-csv-datasource"=  {
        allow_local_mode = true;
      };
      feature_toggles = {
        enable = "publicDashboards";
      };
    };
  };

  services.postgresql = {
    enable = false;
    package = pkgs.postgresql_13;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
    dataDir = "/srv/data/base";
    ensureDatabases = [ "meter" ];
    ensureUsers = [
      { name = "edd"; ensurePermissions = { "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES"; };}
      { name = "meter"; ensurePermissions = { "DATABASE meter" = "ALL PRIVILEGES";};}
    ];
    initialScript = pkgs.writeText "setup-pg" ''
      ALTER ROLE meter WITH LOGIN PASSWORD '${secrets.database.meter.password}';
      ALTER DATABASE meter OWNER to meter;

      CREATE SCHEMA meters;
      CREATE ROLE meterreader;

      GRANT USAGE ON SCHEMA meters TO meter;
      GRANT ALL ON ALL TABLES IN SCHEMA meters TO meter;

      GRANT CONNECT ON DATABASE meter to meterreader;
      GRANT USAGE ON SCHEMA meters TO meterreader;

      CREATE USER grafanareader WITH LOGIN PASSWORD '${secrets.database.grafana-reader.password}';
      GRANT meterreader TO grafanareader;
    '';
  };

  # TODO: put this in a meters project
  # initialSetup = ''
#      CREATE TABLE IF NOT EXISTS meter.meters.temperature (
#        time TIMESTAMPZ,
#        temperature REAL
#      );
#      GRANT SELECT ON meters.temperature TO meterreader;
#      create index IF NOT EXISTS meters_time_brin_idx on meters.temperature using brin (time);
#    '';
  #'';

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
        ''"da-shi.${hosts.domain}  IN  A  ${hosts.ip4 "da-shi"}"''
        ''"draper.${hosts.domain}  IN  A  ${hosts.ip4 "draper"}"''
        ''"blinds.${hosts.domain}  IN  A  ${hosts.ip4 "blinds"}"''
        ] ++ (map
          (s: ''"${s.name}.${hosts.domain}  IN  A  ${hosts.ip4 "da-shi"}"'')
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

  services.nfs.server = let
  in  {
    enable = true;
    exports = ''
        /export            192.168.1.200(rw,fsid=0,no_subtree_check) 192.168.1.222(rw,fsid=0,no_subtree_check)
        /export/books      192.168.1.200(rw,nohide,insecure,no_subtree_check) 192.168.1.222(rw,nohide,insecure,no_subtree_check)
        /export/film       192.168.1.200(rw,nohide,insecure,no_subtree_check) 192.168.1.222(rw,nohide,insecure,no_subtree_check)
    '';
  };

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
    clientMaxBodySize = "20M";
    # other Nginx options;
    #    virtualHosts = with builtins; listToAttrs (map virtualHost hosts.services);
    virtualHosts = with builtins; listToAttrs [
      (virtualHost (elemAt hosts.services 0))
      (virtualHost (elemAt hosts.services 1))
      {
        name = "media.${hosts.domain}";
        value = {
          enableACME = true;
          acmeRoot = null;
          forceSSL = true;
          extraConfig = ''
            # Security / XSS Mitigation Headers
            # NOTE: X-Frame-Options may cause issues with the webOS app
            add_header X-Frame-Options "SAMEORIGIN";
            add_header X-XSS-Protection "0"; # Do NOT enable. This is obsolete/dangerous
            add_header X-Content-Type-Options "nosniff";            

            # Permissions policy. May cause issues on some clients
            add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;

            # Tell browsers to use per-origin process isolation
            add_header Origin-Agent-Cluster "?1" always;

            proxy_headers_hash_max_size 20480;
          '';
          locations."/" = {
            proxyPass = "http://127.0.0.2:8096";
            extraConfig = ''
              proxy_buffering off;
            '';
          };
          locations."/web/" = {
            proxyPass = "http://127.0.0.2:8096/web/index.html";
          };
          locations."/socket" = {
            proxyPass = "http://127.0.0.2:8096";
            proxyWebsockets = true;
          };
          locations."/system/info/public" = {
            proxyPass = "http://127.0.0.2:8096/system/info/public";
          };
        };
      }
    ];
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
      aws s3 sync --only-show-errors --size-only /srv/data/ s3://eddsteel-disk/
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

#   systemd.services.temperature = {
#      wants = ["srv.mount" "network.target" ];
#      serviceConfig.Type = "oneshot";
#      path = [ pkgs.broadlink-cli ];
#      script = ''
#        temp=$(broadlink_cli --type 0x5213 --host 192.168.1.162 --mac ec0baeee04b8 --temperature)
#        dt=$(date -u +"%Y-%m-%d %H:%M:%S")
#        
#        echo "$dt,$temp" >> /srv/data/thermometer
#   '';
#   };
#   systemd.timers.temperature = {
#      wantedBy = [ "timers.target" ];
#      timerConfig = {
#        OnCalendar = "minutely";
#      };
#   };

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
}

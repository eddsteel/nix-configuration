{ lib }:
rec {
  domain = "moron.city";
  consulMain = "da-shi";
  hosts = {
    blinds = [{
      ip4 = "192.168.1.165";
      mac = "E0-4F-43-BC-24-EB";
    }];
    da-shi = [{
      ip4 = "192.168.1.88";
      mac = "9C-2D-CD-6A-BF-D8";
    }];
    draper = [
      {
        ip4 = "192.168.1.200";
        mac = "7C-70-DB-1B-97-70";
      }
      {
        ip4 = "192.168.1.222";
        mac = "A0-CE-C8-76-47-D8";
      }
    ];
    ringo = [{
      ip4 = "192.168.1.203";
      mac = "E8-9F-80-2A-E4-D1";
    }];
    xbox = [{
      ip4 = "192.168.1.99";
      mac = "BC-83-85-A7-BE-61";
    }];
  };
  ip4 = h: (builtins.elemAt hosts."${h}" 0).ip4;
  extraHosts = lib.concatStringsSep "\n" (
    lib.lists.flatten (
      lib.mapAttrsToList (host: ifs:
        (lib.lists.concatMap (inf: ["${inf.ip4} ${host}"]) ifs)
      ) hosts
    )
  );

  services = [
    {"name" = "stats"; "host" = "da-shi"; "url" = "http://da-shi:3000";}
    {"name" = "media"; "host" = "da-shi"; "url" = "http://da-shi:8096";}
    {"name" = "anki"; "host" = "da-shi"; "url" = "http://da-shi:9000";}
  ];
}

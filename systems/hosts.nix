{ lib }:
rec {
  domain = "moron.city";
  consulMain = "gusting";
  hosts = {
    blinds = {
      ip4 = "192.168.1.165";
      mac = "E0-4F-43-BC-24-EB";
    };
    da-shi = {
      ip4 = "192.168.1.88";
      mac = "9C-2D-CD-6A-BF-D8";
    };
    draper = {
      ip4 = "192.168.1.77";
      mac = "7C-70-DB-1B-97-70";
    };
    gusting = {
      ip4 = "192.168.1.33";
      mac = "B8-27-EB-2C-22-55";
    };
    ringo = {
      ip4 = "192.168.1.203";
      mac = "E8-9F-80-2A-E4-D1";
    };
    xbox = {
      ip4 = "192.168.1.99";
      mac = "BC-83-85-A7-BE-61";
    };
  };
  extraHosts = lib.concatStringsSep "\n" (
    lib.mapAttrsToList ( host: attrs: "${attrs.ip4} ${host}") hosts
  );
}

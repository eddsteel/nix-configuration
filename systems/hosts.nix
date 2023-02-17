rec {
  gusting = "192.168.1.39";
  da-shi = "192.168.1.88";
  blinds = "192.168.1.165";
  draper = "192.168.1.200";
  ringo = "192.168.1.203"; # E8-9F-80-2A-E4-D1
  extraHosts = ''
  ${gusting}  gusting
  ${da-shi}  da-shi
  ${blinds} blinds
  ${draper} draper
  ${ringo} ringo
  '';
}

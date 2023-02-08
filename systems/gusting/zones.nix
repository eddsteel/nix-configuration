{pkgs ? (import <nixpkgs> {})}:
with pkgs; let
  lightSwitchFB = fetchurl {
    url = "https://raw.githubusercontent.com/lightswitch05/hosts/ef52c2a80c682e8182102427694640f204a51548/docs/lists/facebook-extended.txt";
    sha256 = "1wsl2if9874wyrgc0byyagqvyf61a7vgn9n0frc43c9giywq306q";
  };
  lightSwitchHate = fetchurl {
    url = "https://raw.githubusercontent.com/lightswitch05/hosts/ef52c2a80c682e8182102427694640f204a51548/docs/lists/hate-and-junk-extended.txt";
    sha256 = "0g44kcxjnvzl0l6mz5lf6353klk11nj8wd95ijnf5hvp6xahyzph";
  };
  lightSwitchAds = fetchurl {
    url = "https://raw.githubusercontent.com/lightswitch05/hosts/ef52c2a80c682e8182102427694640f204a51548/docs/lists/ads-and-tracking-extended.txt";
    sha256 = "1km6flixjv1wzgx0d8frp6q6c6yjf70rxpxkwddidm39xfkkldsi";
  };
  stevenBlackFakeNews = fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/b2d9f7df1ed7e5ba6d103605be3a6155cf028b5b/alternates/fakenews/hosts";
    sha256 = "0sx6dmbrpfh32jmdv1wl069pfsv4nayaassimxymvdxcxjg062rn";
  };
in stdenv.mkDerivation {
  src = ".";
  name = "blocked-zones";
  buildInputs = [ gawk ];
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir $out
    echo "server:" > $out/blocklist.conf
    cat ${lightSwitchFB} \
        ${lightSwitchHate} \
        ${lightSwitchAds} \
        ${stevenBlackFakeNews} | \
        grep ^0.0.0.0 - | \
        grep -v 'clients..google.com$' | \
        grep -v 'android.clients.google.com$' | \
        grep -v 'clientconfig.passport.net$' | \
        grep -v 'data.microsoft.com$' | \
        sed 's/ #.*$//;
        s/^0.0.0.0 \(.*\)/local-zone: "\1" refuse/' | \
        sort -u \
        >> $out/blocklist.conf
  '';
}

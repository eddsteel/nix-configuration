{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati
    vlc
    zoom-us
    ripgrep
    psmisc
  ];

  home.file.".config/zoomus.conf".source = ./files/zoomus.conf;
}

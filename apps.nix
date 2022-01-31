{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati
    ripgrep
    mpv
    unzip
  ];

  home.file.".config/zoomus.conf".source = ./files/zoomus.conf;
}

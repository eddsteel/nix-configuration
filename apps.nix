{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati
    ripgrep
    mpv
  ];

  home.file.".config/zoomus.conf".source = ./files/zoomus.conf;
}

{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati
    ripgrep
  ];

  home.file.".config/zoomus.conf".source = ./files/zoomus.conf;
}

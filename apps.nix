{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati
    jq
    ripgrep
    mpv
    unzip
    awscli2
    aspell
    hub
  ];

  home.file.".config/zoomus.conf".source = ./files/zoomus.conf;
}

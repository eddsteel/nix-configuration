{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
    bitwarden signal-desktop moreutils exfalso
    hub-local
  ];

  xdg.configFile."zoomus.conf".source = ./files/zoomus.conf;
}

{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
    hub bitwarden signal-desktop
  ];

  xdg.configFile."zoomus.conf".source = ./files/zoomus.conf;
}

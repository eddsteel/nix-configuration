{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati jq ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain
    hub bitwarden signal-desktop
  ];

  xdg.configFile."zoomus.conf".source = ./files/zoomus.conf;
}

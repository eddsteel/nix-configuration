{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati ripgrep mpv unzip awscli2 aspell aspellDicts.en git-web-link envchain tree
    moreutils exfalso bitwarden signal-desktop
#    hub-local circleci-cli
  ];

  xdg.configFile."zoomus.conf".source = ./files/zoomus.conf;
}

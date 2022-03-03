{pkgs, ...}:
{
  home.packages = with pkgs; [
    duplicati jq ripgrep mpv unzip awscli2 aspell aspellDicts.en
    hub wavebox bitwarden signal intellij-idea-ce
  ];

  xdg.configFile."zoomus.conf".source = ./files/zoomus.conf;
}

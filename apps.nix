{pkgs, ...}:
{
  home.packages = with pkgs; [
    vlc
    zoom-us
    jq
    ripgrep
    exa
    psmisc
  ];
}

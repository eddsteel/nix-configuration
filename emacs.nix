{ config, pkgs, ...}:
{
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    extraPackages = epkgs: [
      # TODO!
    ];
  };

  xdg.configFile."emacs/init.el" = {
    source = ./files/emacs/init.el;
    onChange = ''
      ${pkgs.my-emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server \
        -e "(load-file user-init-file)"
    '';
  };

  xdg.configFile."emacs/edd" = {
    source = ./files/emacs/edd;
    onChange = ''
      ${pkgs.my-emacs}/bin/emacsclient --no-wait --socket=${config.home.homeDirectory}/run/emacs/server \
        -e "(load-file user-init-file)"
    '';
  };
}

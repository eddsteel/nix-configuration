{pkgs, config, localPkgs, ...} :
let {
  localPkgs = import ./local.nix { inherit pkgs; };
}; in {
  programs.gpg = {
    enable = true;
    settings = {
      "require-cross-certification" = true;
      "keyserver" = ["hkp://keys.gnupg.net" "http://http-keys.gnupg.net"];
      "keyserver-options" = " auto-key-retrieve";
      "use-agent" = true;
    };
  };
  services.gpg-agent = {
    enable = true;
    extraConfig = ''
    default-cache-ttl 3600
    allow-emacs-pinentry
    '';
  };

  home.activation."importKeys" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    gpg --import ${gpgPub}
    gpg --import ${gpgSec}
  '';
  systemd.user = {
    tmpfiles.rules = [
      "e ${config.home.homeDirectory}/tmp                0755 edd users 10d -"
    ];
    startServices = true;

    services.brainzo-api = {
      Unit = { Description = "brainzo-api"; };
      Install = { WantedBy = ["default.target"]; };
      Service = {
        Type = "simple";
        Restart = "always";
        ExecStart = "${localPkgs.brainzo}/bin/brainzo-api";
        KillMode = "process";
        TimeoutSec = 180;
      };
    };
  };

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      onepassword-password-manager anchors-reveal auto-tab-discard
      duckduckgo-privacy-essentials
    ];
    # TODO use the gnome one (we get it from firefox sync currently anyway)
    profiles."default" = {
      id = 0;
      path = "xtqfr4qa.default";
      isDefault = true;
      settings = {
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        "browser.warnOnQuitShortcut" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "services.sync.username" = "edd@eddsteel.com";
        "services.sync.engine.creditcards" = false;
        "services.sync.engine.passwords" = false;
        "accessibility.typeaheadfind.enablesound" = false;
      };
    };
  };

  home.packages = (lib.attrsets.attrValues localPkgs) # i.e. all defined locally-built packages.
}

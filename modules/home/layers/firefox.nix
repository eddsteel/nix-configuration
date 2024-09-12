{ config, pkgs, lib, ... }:
let
  cfg = config.layers.firefox;
in with lib; {
  options.layers.firefox = {
    enable = mkEnableOption "Standard firefox configuration";
    sync-user = mkOption {};
    downloads = mkOption { default = "${config.home.homeDirectory}/tmp"; };
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      profiles."default" = {
        id = 0;
        path = "xtqfr4qa.default-release";
        isDefault = true;
        settings = {
          "app.normandy.first_run" = false;
          "beacon.enabled" = false;
          "browser.aboutConfig.showWarning" = false;
          "browser.download.dir" = cfg.downloads;
          "browser.startup.homepage" = "about:blank";
          "browser.newtabpage.enabled" = false;
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.feeds.section.topsites" = false;
          "browser.warnOnQuitShortcut" = false;
          "browser.warnOnCloseOtherTabs" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "services.sync.username" = cfg.sync-user;
          "services.sync.engine.creditcards" = false;
          "services.sync.engine.passwords" = false;
          "services.sync.prefs.sync.browser.tabs.warnOnClose" = false;
          "services.sync.declinedEngines" = "passwords";
          "servaices.sync.rememberSignons" = false;
          "accessibility.typeaheadfind.enablesound" = false;
          "signon.rememberSignons" = false;
          "signon.autofillForms" = false;
          "signon.generation.enabled" = false;
        };
      };
    };
  };
}

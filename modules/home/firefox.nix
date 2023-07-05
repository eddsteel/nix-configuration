{ config, pkgs, lib, ... }:
let
  cfg = config.firefox;
in with lib; {
  options.firefox = {
    enable = mkEnableOption "Standard firefox configuration";
    sync-user = mkOption {};
    downloads = mkOption { default = "${config.home.homeDirectory}/tmp"; };
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      profiles."default" = {
        id = 0;
        path = "xtqfr4qa.default";
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          anchors-reveal auto-tab-discard duckduckgo-privacy-essentials bitwarden
        ];
        isDefault = true;
        settings = {
          "browser.aboutConfig.showWarning" = false;
          "browser.download.dir" = cfg.downloads;
          "browser.startup.homepage" = "about:blank";
          "browser.newtabpage.enabled" = false;
          "browser.tabs.firefox-view" = false;
          "browser.warnOnQuitShortcut" = false;
          "browser.warnOnCloseOtherTabs" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "services.sync.username" = cfg.sync-user;
          "services.sync.engine.creditcards" = false;
          "services.sync.engine.passwords" = false;
          "services.sync.prefs.sync.browser.tabs.warnOnClose" = false;
          "accessibility.typeaheadfind.enablesound" = false;
          "signon.rememberSignons" = false;
          "signon.autofillForms" = false;
          "signon.generation.enabled" = false;
        };
      };
    };
  };
}

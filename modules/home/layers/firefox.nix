{ config, pkgs, lib, ... }:
let
  cfg = config.layers.firefox;
  profile = "uooem51c.default-release-5";
in with lib; {
  options.layers.firefox = {
    enable = mkEnableOption "Standard firefox configuration";
    sync-user = mkOption {};
    downloads = mkOption { default = "${config.home.homeDirectory}/tmp"; };
    profile = mkOption {};
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.firefox];
    home.file."Library/Application Support/Firefox/Profiles/${cfg.profile}/user.js".text = ''
user_pref("app.normandy.first_run", false);
user_pref("beacon.enabled", false);
user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.download.dir", "${cfg.downloads}");
user_pref("browser.startup.homepage", "about:blank");
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topsites", false);
user_pref("browser.warnOnQuitShortcut", false);
user_pref("browser.warnOnClose", false);
user_pref("browser.warnOnCloseOtherTabs", false);
user_pref("extensions.formautofill.creditCards.enabled", false);
user_pref("browser.urlbar.suggest.topsites", false);
user_pref("browser.urlbar.suggest.addons", false);
user_pref("browser.urlbar.suggest.clipboard", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("browser.urlbar.suggest.fakespot", false);
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);
user_pref("browser.urlbar.suggest.pocket", false);
user_pref("browser.urlbar.suggest.searches", false);
user_pref("browser.urlbar.suggest.recentsearches", false);
user_pref("browser.urlbar.suggest.topsites", false);
user_pref("browser.urlbar.suggest.trending", false);
user_pref("browser.urlbar.suggest.weather", false);
user_pref("browser.urlbar.suggest.yelp", false);
user_pref("services.sync.username", "${cfg.sync-user}");
user_pref("services.sync.engine.creditcards", false);
user_pref("services.sync.engine.passwords", false);
user_pref("services.sync.prefs.sync.browser.tabs.warnOnClose", false);
user_pref("services.sync.declinedEngines", "passwords");
user_pref("services.sync.rememberSignons", false);
user_pref("services.sync.prefs.sync.browser.urlbar.suggest.searches", false);
user_pref("accessibility.typeaheadfind.enablesound", false);
user_pref("signon.rememberSignons", false);
user_pref("signon.autofillForms", false);
user_pref("signon.generation.enabled", false);
    '';
  };
}

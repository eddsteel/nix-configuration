{ config, pkgs, lib, ... }:
let
  cfg = config.layers.firefox;
  script = pkgs.writers.writePython3 "fix-firefox-py" {} ''
import re
import sys
import os

path, target = sys.argv[1], sys.argv[2]

profiles_path = f'{path}/profiles.ini'
installs_path = f'{path}/installs.ini'


def fix_install_sections(content, target):
    sections = re.split(r'(?=^\[)', content, flags=re.MULTILINE)
    result = []
    changed = False
    for section in sections:
        if re.match(r'^\[Install', section):
            new = re.sub(
                         r'^Default=.*$',
                         f'Default={target}',
                         section,
                         flags=re.MULTILINE)
            if new != section:
                changed = True
            elif 'Default=' not in section:
                new = new.rstrip('\n') + f'\nDefault={target}\nLocked=1\n\n'
                changed = True
            section = new
            if 'Locked=' not in section:
                section = section.rstrip('\n') + '\nLocked=1\n\n'
                changed = True
        result.append(section)
    return "".join(result), changed


with open(profiles_path) as f:
    content = f.read()
fixed, changed = fix_install_sections(content, target)
if changed:
    with open(profiles_path, 'w') as f:
        f.write(fixed)
    print('Fixed Firefox profiles.ini')

if os.path.exists(installs_path):
    with open(installs_path) as f:
        content = f.read()
    # installs.ini uses bare section headers like [HEXID], not [InstallHEXID]
    # Rewrite all sections unconditionally
    sections = re.split(r'(?=^\[)', content, flags=re.MULTILINE)
    result = []
    changed = False
    for section in sections:
        if re.match(r'^\[[0-9A-Fa-f]+\]', section):
            new = re.sub(
                         r'^Default=.*$',
                         f'Default={target}',
                         section,
                         flags=re.MULTILINE)
            if new != section:
                changed = True
            elif 'Default=' not in section:
                new = new.rstrip('\n') + f'\nDefault={target}\nLocked=1\n\n'
                changed = True
            section = new
            if 'Locked=' not in section:
                section = section.rstrip('\n') + '\nLocked=1\n\n'
                changed = True
        result.append(section)
    if changed:
        with open(installs_path, 'w') as f:
            f.write("".join(result))
        print('Fixed Firefox installs.ini')
  '';
  fixFirefoxProfile = pkgs.writeShellScript "fix-firefox-profile" ''
    case "$(uname)" in
      Darwin) dir="$HOME/Library/Application Support/Firefox" ;;
      *)      dir="$HOME/.mozilla/firefox" ;;
    esac
    target="Profiles/${cfg.profile}"

    ${pkgs.python3}/bin/python3 ${script} "$dir" "$target"
'';
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
user_pref("browser.search.suggest.enabled", false);
user_pref("services.sync.username", "${cfg.sync-user}");
user_pref("services.sync.engine.creditcards", false);
user_pref("services.sync.engine.passwords", false);
user_pref("services.sync.prefs.sync.browser.tabs.warnOnClose", false);
user_pref("services.sync.declinedEngines", "passwords");
user_pref("services.sync.rememberSignons", false);
user_pref("services.sync.prefs.sync.browser.urlbar.suggest.searches", false);
user_pref("sidebar.verticalTabs", true);
user_pref("sidebar.verticalTabs.dragToPinPromo.dismissed", true)
user_pref("accessibility.typeaheadfind.enablesound", false);
user_pref("signon.rememberSignons", false);
user_pref("signon.autofillForms", false);
user_pref("signon.generation.enabled", false);
user_pref("browser.ml.chat.enabled", false);
    '';
    home.activation.fixFirefoxProfile = lib.hm.dag.entryAfter [ "writeBoundary" ] ''${fixFirefoxProfile}'';
  };
}

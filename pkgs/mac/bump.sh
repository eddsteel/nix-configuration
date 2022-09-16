#!/bin/sh
# TODO: use published SHAs not pre-fetched.

set -e
touch versions.json
rm -f .*-component

location() {
    curl -Is "$1" | awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}' | head -n 1
}

location_get() {
    curl -Is -XGET "$1" | awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}'
}

conditional_get_sha() {
    if [ "$(jq -r .$1.url versions.json)" != "$2" ]; then
        nix-prefetch-url --name "$3" "$2"
    else
        jq -r ".$1.sha256" versions.json
    fi
}

component_json() {
    jq -nc --arg url "$1" --arg sha "$2" --arg name "$3" --arg ver "$4"\
       '{"name": $name, "sha256": $sha, "url": $url, "version": $ver}' > ".$5-component"
}

standard() {
    URL=${URL-$(location "$1")}
    VER=$(echo $URL | sed "s!$2!\1!")
    NME="$3-$VER.$4"
    SHA=$(conditional_get_sha $5 "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" $6
}

github() {
    url=$(location "https://github.com/$1/releases/latest")
    VER=$(echo $url | sed 's!^.*/tag/v\([-0-9.a-zA-Z]*\)$!\1!')
    NME="$2$VER.dmg"
    URL="https://github.com/$1/releases/download/v$VER/$NME"
    SHA=$(conditional_get_sha $3 "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" $4
}

wavebox() {
    standard "https://download.wavebox.app/latest/stable/macuniversal" \
             '^.*Wavebox%20\(.*\).dmg$' \
             "wavebox" "dmg" "wavebox" "wb"
}

bitwarden() {
    BW_URL=$(location_get 'https://vault.bitwarden.com/download/?app=desktop&platform=macos&variant=dmg')
    URL=$BW_URL standard '#' '^.*/Bitwarden-\([^-]*\)-universal.dmg$' \
                "bitwarden" "dmg" "bitwarden" "bw"
}

iterm2() {
    IT_URL=$(location "https://iterm2.com/downloads/stable/latest")
    IT_VER=$(echo "$IT_URL" | sed 's!.*/iTerm2-\([0-9_]*\).zip$!\1!' | sed 's/_/./g')
    IT_NME="iterm2-$IT_VER.zip"
    IT_SHA=$(conditional_get_sha iterm2 "$IT_URL" "$IT_NME")
    component_json "$IT_URL" "$IT_SHA" "$IT_NME" "$IT_VER" it
}

firefox() {
    standard 'https://download.mozilla.org/?product=firefox-latest-ssl&os=osx&lang=en-CA' \
             '^.*/releases/\([0-9.]*\)/mac/.*$' \
             "Firefox" "dmg" "firefox" "ff"
}

idea() {
    url=$(location 'https://data.services.jetbrains.com/products/download?code=IIC&platform=mac')
    url2=$(echo $url | sed 's/.dmg$/-aarch64.dmg/')
    URL="$url2" standard '#' '^.*/ideaIC-\([0-9.]*\)-aarch64.dmg$' \
             "intellij-idea-ce" "dmg" "idea" ij
}

signal() {
    SN_NME=$(curl -s https://updates.signal.org/desktop/latest-mac.yml | grep -Eo 'signal-desktop-mac-universal.*.dmg$')
    SN_URL="https://updates.signal.org/desktop/$SN_NME"
    SN_VER=$(echo $SN_NME | sed 's!^.*mac-universal-\([0-9.]*\).dmg$!\1!')
    SN_SHA=$(conditional_get_sha signal "$SN_URL" "$SN_NME")
    component_json "$SN_URL" "$SN_SHA" "$SN_NME" "$SN_VER" sn
}

istat() {
    standard 'https://download.bjango.com/istatmenus/' \
             '^.*/istatmenus\([0-9.]*\).zip$' \
             "istat" "zip" "istatmenus" im
}

rectangle() {
    github "rxhanson/Rectangle" "Rectangle" "rectangle" "re"
}

xbar() {
    github "matryer/xbar" "xbar.v" "xbar" "xb"
}

exfalso() {
    # update github to allow (v-/release-)
    url="https://github.com/quodlibet/quodlibet/releases/tag/release-4.4.0" # $(location "https://github.com/$1/releases/latest")
    VER="4.4.0" # $(echo $url | sed 's!^.*/tag/release-\([-0-9.a-zA-Z]*\)$!\1!')
    NME="ExFalso-$VER.dmg"
    URL="https://github.com/quodlibet/quodlibet/releases/download/release-$VER/$NME"
    SHA=$(conditional_get_sha exfalso "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" ef
}

wavebox &
bitwarden &
iterm2 &
firefox &
idea &
signal &
istat &
rectangle &
xbar &
exfalso &

wait

jq -n \
   --slurpfile wb .wb-component \
   --slurpfile bw .bw-component \
   --slurpfile it .it-component \
   --slurpfile ff .ff-component \
   --slurpfile ij .ij-component \
   --slurpfile sn .sn-component \
   --slurpfile im .im-component \
   --slurpfile re .re-component \
   --slurpfile xb .xb-component \
   --slurpfile ef .ef-component \
   '{ "wavebox": $wb[0], "bitwarden": $bw[0], "iterm2": $it[0], "firefox": $ff[0], "idea": $ij[0], "signal": $sn[0], "istatmenus": $im[0], "rectangle": $re[0], "xbar": $xb[0], "exfalso": $ef[0]}' \
   >new.json

rm .*-component
mv new.json versions.json

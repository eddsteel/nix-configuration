#!/bin/sh
# TODO: use published SHAs not pre-fetched.

set -e
touch versions.json
rm -f .*-component

location() {
    curl -Is "$1" | awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}'
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

wavebox() {
    WB_URL=$(location "https://download.wavebox.app/latest/stable/macuniversal")
    WB_VER=$(echo "$WB_URL" | sed 's/^.*Wavebox%20\(.*\).dmg$/\1/')
    WB_NME="wavebox-$WB_VER.dmg"
    WB_SHA=$(conditional_get_sha wavebox "$WB_URL" "$WB_NME")
    component_json "$WB_URL" "$WB_SHA" "$WB_NME" "$WB_VER" wb
}

bitwarden() {
    BW_URL=$(location_get 'https://vault.bitwarden.com/download/?app=desktop&platform=macos&variant=dmg')
    BW_VER=$(echo "$BW_URL" | sed 's!^.*/Bitwarden-\([^-]*\)-universal.dmg$!\1!')
    BW_NME="bitwarden-$BW_VER.dmg"
    BW_SHA=$(conditional_get_sha bitwarden "$BW_URL" "$BW_NME")
    component_json "$BW_URL" "$BW_SHA" "$BW_NME" "$BW_VER" bw
}

iterm2() {
    IT_URL=$(location "https://iterm2.com/downloads/stable/latest")
    IT_VER=$(echo "$IT_URL" | sed 's!.*/iTerm2-\([0-9_]*\).zip$!\1!' | sed 's/_/./g')
    IT_NME="iterm2-$IT_VER.zip"
    IT_SHA=$(conditional_get_sha iterm2 "$IT_URL" "$IT_NME")
    component_json "$IT_URL" "$IT_SHA" "$IT_NME" "$IT_VER" it
}

firefox() {
    FF_URL=$(location 'https://download.mozilla.org/?product=firefox-latest-ssl&os=osx&lang=en-CA')
    FF_VER=$(echo "$FF_URL" | sed 's!^.*/releases/\([0-9.]*\)/mac/.*$!\1!')
    FF_NME="Firefox-$FF_VER.dmg"
    FF_SHA=$(conditional_get_sha firefox "$FF_URL" "$FF_NME")
    component_json "$FF_URL" "$FF_SHA" "$FF_NME" "$FF_VER" ff
}

idea() {
    # note: for apple silicon insert '-aarch64' before '.dmg'
    IJ_URL=$(location 'https://data.services.jetbrains.com/products/download?code=IIC&platform=mac')
    IJ_VER=$(echo "$IJ_URL" | sed 's!^.*/ideaIC-\([0-9.]*\).dmg$!\1!')
    IJ_NME="intellij-idea-ce-$IJ_VER.dmg"
    IJ_SHA=$(conditional_get_sha idea "$IJ_URL" "$IJ_NME")
    component_json "$IJ_URL" "$IJ_SHA" "$IJ_NME" "$IJ_VER" ij
}

signal() {
    SN_NME=$(curl -s https://updates.signal.org/desktop/latest-mac.yml | grep -Eo 'signal-desktop-mac-universal.*.dmg$')
    SN_URL="https://updates.signal.org/desktop/$SN_NME"
    SN_VER=$(echo $SN_NME | sed 's!^.*mac-universal-\([0-9.]*\).dmg$!\1!')
    SN_SHA=$(conditional_get_sha signal "$SN_URL" "$SN_NME")
    component_json "$SN_URL" "$SN_SHA" "$SN_NME" "$SN_VER" sn
}

wavebox &
bitwarden &
iterm2 &
firefox &
idea &
signal &

wait

jq -n \
   --slurpfile wb .wb-component \
   --slurpfile bw .bw-component \
   --slurpfile it .it-component \
   --slurpfile ff .ff-component \
   --slurpfile ij .ij-component \
   --slurpfile sn .sn-component \
   '{ "wavebox": $wb[0], "bitwarden": $bw[0], "iterm2": $it[0], "firefox": $ff[0], "idea": $ij[0], "signal": $sn[0]}' \
   >new.json

rm .*-component
mv new.json versions.json

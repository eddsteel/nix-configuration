#!/bin/sh
rm -f .components

wavebox() {
    WB_URL=$(curl -sI https://download.wavebox.app/latest/stable/macuniversal | \
                 awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}')
    WB_VER=$(echo "$WB_URL" | sed 's/^.*Wavebox%20\(.*\).dmg$/\1/')
    WB_SHA=$(jq -r .wavebox.sha256 versions.json)
    WB_NME="wavebox-$WB_VER.dmg"
    if [ $(jq -r .wavebox.url versions.json) != "$WB_URL" ]; then
        WB_SHA=$(nix-prefetch-url --name "$WB_NME" "$WB_URL")
    fi

    jq -nc --arg url "$WB_URL" --arg sha "$WB_SHA" --arg name "$WB_NME" --arg ver "$WB_VER"\
       '{"name": $name, "sha256": $sha, "url": $url, "version": $ver}' >> .components
}

bitwarden() {
    BW_URL=$(curl -sI -XGET 'https://vault.bitwarden.com/download/?app=desktop&platform=macos&variant=dmg' | \
                 awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}')
    BW_VER=$(echo "$BW_URL" | sed 's!^.*/Bitwarden-\([^-]*\)-universal.dmg$!\1!')
    BW_SHA=$(jq -r .bitwarden.sha256 versions.json)
    BW_NME="bitwarden-$BW_VER.dmg"
    if [ $(jq -r .bitwarden.url versions.json) != "$BW_URL" ]; then
        BW_SHA=$(nix-prefetch-url --name "$BW_NME" "$BW_URL")
    fi

    jq -nc --arg url "$BW_URL" --arg sha "$BW_SHA" --arg name "$BW_NME" --arg ver "$BW_VER"\
       '{"name": $name, "sha256": $sha, "url": $url, "version": $ver}' >> .components
}

iterm2() {
    IT_URL=$(curl -Is https://iterm2.com/downloads/stable/latest | \
                 awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}')
    IT_VER=$(echo "$IT_URL" | sed 's!.*/iTerm2-\([0-9_]*\).zip$!\1!' | sed 's/_/./g')
    IT_NME="iterm2-$IT_VER.zip"
    IT_SHA=$(jq -r .iterm2.sha256 versions.json)
    if [ $(jq -r .iterm2.url versions.json) != "$IT_URL" ]; then
        IT_SHA=$(nix-prefetch-url --name "$IT_NME" "$IT_URL")
    fi

    jq -nc --arg url "$IT_URL" --arg sha "$IT_SHA" --arg name "$IT_NME" --arg ver "$IT_VER"\
       '{"name": $name, "sha256": $sha, "url": $url, "version": $ver}' >> .components

}

firefox() {
    FF_URL=$(curl -sI 'https://download.mozilla.org/?product=firefox-latest-ssl&os=osx&lang=en-CA' | \
                 awk -v FS=": " 'BEGIN{RS="\r\n";}/[lL]ocation/{print $2}')
    FF_VER=$(echo "$FF_URL" | sed 's!^.*/releases/\([0-9.]*\)/mac/.*$!\1!')
    FF_NME="Firefox-$FF_VER.dmg"
    FF_SHA=$(jq -r .firefox.sha256 versions.json)
    if [ $(jq -r .firefox.url versions.json) != "$FF_URL" ]; then
        FF_SHA=$(nix-prefetch-url --name "$FF_NME" "$FF_URL")
    fi

    jq -nc --arg url "$FF_URL" --arg sha "$FF_SHA" --arg name "$FF_NME" --arg ver "$FF_VER"\
       '{"name": $name, "sha256": $sha, "url": $url, "version": $ver}' >> .components
}

wavebox
bitwarden
iterm2
firefox

jq -n --slurpfile components .components \
   '{"wavebox": $components[0], "bitwarden": $components[1], "iterm2": $components[2], "firefox": $components[3]}' \
   > new.json

rm .components
mv new.json versions.json

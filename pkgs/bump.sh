#!/bin/sh
# TODO: use GNU parallel with -k instead of temporary files to output to versions.json directly.

set -e
touch versions.json
rm -f .*-component

location() {
    header "$1" 'location'
}

cfversion() {
    header "$1" x-amz-meta-version
}


location_get() {
    curl -Is -XGET "$1" -o /dev/null -w '%header{location}'
}

header() {
    curl --insecure -Is "$1" -o /dev/null -w "%header{$2}"
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
    VER=$(echo $url | sed "s!^.*/tag/$5\([-0-9.a-zA-Z]*\)\$!\1!")
    NME="$2$VER$6"
    URL="https://github.com/$1/releases/download/$5$VER/$NME"
    SHA=$(conditional_get_sha $3 "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" $4
}

#--

awsvpn() {
    URL="https://d20adtppz83p9s.cloudfront.net/OSX/latest/AWS_VPN_Client.pkg"
    VER=$(cfversion $URL)
    NME="AWS_VPN_Client-${VER}.pkg"
    SHA=$(conditional_get_sha awsvpn "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" av
}

wavebox() {
    if [ "$1" = "darwin" ]; then
        standard "https://download.wavebox.app/latest/stable/macuniversal" \
                 '^.*Wavebox%20\(.*\).dmg$' \
                 "wavebox" "dmg" "wavebox.darwin" "wbd"
    else
        standard "https://download.wavebox.app/latest/stable/linux/tar" \
                 '^.*Wavebox_\(.*\).tar.gz$' \
                 "wavebox" "tar.gz" "wavebox.linux" "wbl"
    fi
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
xbar() {
    github "matryer/xbar" "xbar.v" "xbar" "xb" "v" ".dmg"
}

tdocs() {
    github "terraform-docs/terraform-docs" "terraform-docs-v" "terraform_docs" "td" "v" "-darwin-amd64.tar.gz"
}

circleci() {
    if [ "$1" = "darwin" ]; then
        github "CircleCI-Public/circleci-cli" "circleci-cli_" "circleci_cli.darwin" "ccd" "v" \
               "_darwin_amd64.tar.gz"
    else
        github "CircleCI-Public/circleci-cli" "circleci-cli_" "circleci_cli.linux" "ccl" "v" \
               "_linux_amd64.tar.gz"
    fi
}

exfalso() { # 4.4.0 was the last with a dmg release.
    url="https://github.com/quodlibet/quodlibet/releases/tag/release-4.4.0" # $(location "https://github.com/$1/releases/latest")
    VER="4.4.0" # $(echo $url | sed 's!^.*/tag/release-\([-0-9.a-zA-Z]*\)$!\1!')
    NME="ExFalso-$VER.dmg"
    URL="https://github.com/quodlibet/quodlibet/releases/download/release-$VER/$NME"
    SHA=$(conditional_get_sha exfalso "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" ef
}

caffeine() {
    url=$(location "https://github.com/IntelliScape/caffeine/releases/latest")
    VER=$(echo $url | sed "s!^.*/tag/\([-0-9.a-zA-Z]*\)\$!\1!")
    NME="Caffeine.dmg"
    URL="https://github.com/IntelliScape/caffeine/releases/download/$VER/$NME"
    SHA=$(conditional_get_sha caffeine "$URL" "$NME")
    component_json "$URL" "$SHA" "$NME" "$VER" cf
}

zoomus() {
    if [ $1 == "linux" ]; then
        VER=$(curl -Ls 'https://zoom.us/rest/download?os=linux' | jq -r .result.downloadVO.zoom.version)
        URL="https://zoom.us/client/$VER/zoom_x86_64.pkg.tar.xz"
        NME="zoom_x86_64.pkg.tar.xz"
        SHA=$(conditional_get_sha "zoom_us.$1" "$URL" "$NME")
        component_json "$URL" "$SHA" "$NME" "$VER" zul
    else
        VER=$(curl -Ls 'https://zoom.us/rest/download?os=mac' | jq -r .result.downloadVO.zoomArm64.version)
        URL="https://zoom.us/client/$VER/zoomusInstallerFull.pkg?archType=arm64"
        NME=zoomusInstallerFull.pkg
        SHA=$(conditional_get_sha "zoom_us.$1" "$URL" "$NME")
        component_json "$URL" "$SHA" "$NME" "$VER" zud
    fi
}

orbstack() {
    standard "https://orbstack.dev/download/stable/latest/arm64" \
             '^.*OrbStack_v\(.*\)_arm64.dmg$' \
             "orbstack" "dmg" "orbstack" "os"
}

podman() {
    github "containers/podman-desktop" "podman-desktop-" "podman" "pm" "v" "-universal.dmg"
}

wavebox linux &
wavebox darwin &
bitwarden &
firefox &
idea &
signal &
istat &
xbar &
exfalso &
caffeine &
circleci linux &
circleci darwin &
zoomus linux &
zoomus darwin &
tdocs &
awsvpn &
orbstack &
podman &

wait

jq -n \
   --slurpfile av .av-component \
   --slurpfile wbd .wbd-component \
   --slurpfile wbl .wbl-component \
   --slurpfile bw .bw-component \
   --slurpfile ff .ff-component \
   --slurpfile ij .ij-component \
   --slurpfile sn .sn-component \
   --slurpfile im .im-component \
   --slurpfile xb .xb-component \
   --slurpfile ef .ef-component \
   --slurpfile cf .cf-component \
   --slurpfile ccd .ccd-component \
   --slurpfile ccl .ccl-component \
   --slurpfile zud .zud-component \
   --slurpfile zul .zul-component \
   --slurpfile td .td-component \
   --slurpfile os .os-component \
   --slurpfile pm .pm-component \
   '{ "wavebox": {"darwin": $wbd[0], "linux": $wbl[0]}, "bitwarden": $bw[0], "firefox": $ff[0], "idea": $ij[0], "signal": $sn[0], "istatmenus": $im[0], "xbar": $xb[0], "exfalso": $ef[0], "caffeine": $cf[0], "circleci_cli": {"darwin": $ccd[0], "linux": $ccl[0]}, "zoom_us": {"darwin": $zud[0], "linux": $zul[0]}, "terraform_docs": $td[0], "awsvpn": $av[0], "orbstack": $os[0], "podman": $pm[0]}' \
   >new.json

rm .*-component
mv new.json versions.json

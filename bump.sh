#!/bin/sh
set -e

header() {
    curl --insecure -Is "$1" -o /dev/null -w "%header{$2}"
}

location() {
    header "$1" 'location'
}

conditional_pin() {
    current=$(npins show $1 | grep url: | sed 's/^ *url: *//')
    if [ ".$current." != ".$2." ]; then
        npins add url $2 --name $1
    fi
}

pin_github() {
    url=$(location "https://github.com/$2/releases/latest")
    version=$(echo $url | sed "s!^.*/tag/v\?\([-0-9.a-zA-Z]*\)\$!\1!")
    conditional_pin $1 "https://github.com/$2/$(printf $3 $version $version)"
}

podman() {
    pin_github podman "podman-desktop/podman-desktop" \
      "releases/download/v%s/podman-desktop-%s-universal.dmg"
}

xbar() {
    pin_github xbar "matryer/xbar" "releases/download/v%s/xbar.v%s.dmg"
}

tdocs() {
    pin_github terraform-docs "terraform-docs/terraform-docs" \
               "releases/download/v%s/terraform-docs-v%s-darwin-arm64.tar.gz"
}

caffeine() {
    url=$(location "https://github.com/Intelliscape/caffeine/releases/latest")
    version=$(echo $url | sed "s!^.*/tag/v\?\([-0-9.a-zA-Z]*\)\$!\1!")
    conditional_pin caffeine "https://github.com/Intelliscape/caffeine/releases/download/$version/Caffeine.dmg"
}

podman &
xbar &
tdocs &
caffeine &

wait

npins update
npins update -f awsvpn

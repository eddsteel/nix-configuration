#!/usr/bin/env sh

WD=$(dirname $(realpath $0))

function die() {
    echo $1
    exit 1
}

test -n "$HOSTNAME" || die "\$HOSTNAME is required"

function bootstrapDarwin() {
    which darwin-rebuild || die "nix-darwin is required"
    darwin-rebuild switch \
                   -I "darwin-config=$WD/systems/$HOSTNAME/darwin.nix" \
                   -I "nix-config=$WD"
}

function bootstrapDarwinHome() {
    which home-manager || die "home-manager is required"
    NIXPKGS_CONFIG="$WD/nixpkgs.nix" home-manager switch \
                  -I "darwin-config=$WD/systems/$HOSTNAME/darwin.nix" \
                  -I "nix-config=$WD" \
                  -I "nixpkgs-config=$WD/nixpkgs.nix" \
                  -I "hm-config=$WD/systems/$HOSTNAME/home.nix" \
                  -I "nixpkgs-overlays=$WD/overlays.nix"
}

function bootstrapNixOS() {
    which nixos-rebuild sudo || die "sudo and nixos-rebuild are required"
    sudo nixos-rebuild switch \
                  -I "nixos-config=$WD/systems/$HOSTNAME/nixos.nix" \
                  -I "nix-config=$WD"
}

function bootstrapNixHM() {
    which home-manager || die "home-manager is required"
    NIXPKGS_CONFIG="$WD/nixpkgs.nix" home-manager switch \
                  -I "nix-config=$WD" \
                  -I "nixpkgs-config=$WD/nixpkgs.nix" \
                  -I "hm-config=$WD/systems/$HOSTNAME/home.nix" \
                  -I "nixpkgs-overlays=$WD/overlays.nix"
}

# TODO: put nix-env with git + cloning + installing darwin/home-manager here?

case $(uname -a) in
    Darwin*) bootstrapDarwin && bootstrapDarwinHome;;
    Linux*NixOS*) bootstrapNixOS && bootstrapNixHM;;
    Linux) bootstrapNixHM;;
esac

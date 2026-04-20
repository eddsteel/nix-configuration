#!/usr/bin/env sh

WD=$(dirname $(realpath $0))

function die() {
    echo $1
    exit 1
}

test -n "$HOSTNAME" || die "\$HOSTNAME is required"

function bootstrapDarwin() {
    nix-build $(nix-instantiate --raw --eval npins -A darwin) -A darwin-rebuild
    sudo ./result/bin/darwin-rebuild switch -I "darwin-config=$WD/systems/$HOSTNAME/darwin.nix" -I "nix-config=$WD"
    rm result
}

function bootstrapDarwinHome() {
    NIX_PATH="darwin-config=$WD/systems/$HOSTNAME/darwin.nix:nix-config=$WD:nixpkgs-config=$WD/nixpkgs.nix:hm-config=$WD/systems/$HOSTNAME/home.nix:nixpkgs-overlays=$WD/overlays" \
    NIXPKGS_CONFIG="$WD/nixpkgs.nix" nix-shell -p home-manager --command "home-manager switch -f $WD/systems/$HOSTNAME/home.nix"
 }

function bootstrapNixOS() {
    which nixos-rebuild sudo || die "sudo and nixos-rebuild are required"
    NIXOS_CONFIG="$WD/systems/$HOSTNAME/nixos.nix" sudo nixos-rebuild switch \
                  -I "nixos-config=$WD/systems/$HOSTNAME/nixos.nix" \
                  -I "nix-config=$WD"
}

function bootstrapNixHM() {
    NIX_PATH="nix-config=$WD:nixpkgs-config=$WD/nixpkgs.nix:hm-config=$WD/systems/$HOSTNAME/home.nix:nixpkgs-overlays=$WD/overlays" \
    NIXPKGS_CONFIG="$WD/nixpkgs.nix" nix-shell -p home-manager --command "home-manager switch -f $WD/systems/$HOSTNAME/home.nix"
}

case $(uname -a) in
    Darwin*) bootstrapDarwin && bootstrapDarwinHome;;
    Linux*NixOS*) bootstrapNixOS && bootstrapNixHM;;
    Linux) bootstrapNixHM;;
esac

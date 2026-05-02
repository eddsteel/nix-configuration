#!/bin/sh

home-manager expire-generations '-9 days'
nix-collect-garbage

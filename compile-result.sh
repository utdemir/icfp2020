#!/usr/bin/env sh

drv="$(nix-build -A utdemir-icfp2020)"
cp "$drv/bin/utdemir-icfp2020" compiled/

#!/bin/bash

sudo pacman -S - < arch_packages.txt

gpg --recv-keys 58117AFA1F85B3EEC154677D615D449FE6E6A235
gpg --recv-keys 63CBEEC9006602088F9B19326224F9941A8AA6D1
gpg --recv-keys E46E6F8FF02E4C83569084589239277F560C95AC

pikaur -S - < aur_packages.txt



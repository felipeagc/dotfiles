#!/bin/bash

sudo pacman -S --needed - < arch_packages.txt

pikaur -S --needed - < aur_packages.txt

elephant service enable
systemctl enable --user elephant
systemctl start --user elephant

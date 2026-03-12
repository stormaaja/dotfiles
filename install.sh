#!/bin/sh

dotfiles=$(pwd)

echo "Setting nvim config"
mkdir -p $HOME/.config/nvim
cd $HOME/.config/nvim

if [ ! -f init.lua ]; then
  ln -s "${dotfiles}/config/nvim/init.lua"
fi

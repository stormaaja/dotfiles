#!/bin/sh

dotfiles=$(pwd)

declare -A config_files=(
  [nvim]="init.lua"
  [sway]="config"
	[foot]="foot.ini"
)

for config_name in "${!config_files[@]}"; do
  files=${config_files[$config_name]}
  echo "Setting ${config_name} config"
  mkdir -p $HOME/.config/${config_name}
  cd $HOME/.config/${config_name}
  for file in ${files[@]}; do
    if [ ! -f ${file} ]; then
      ln -s "${dotfiles}/config/${config_name}/${file}"
    fi
  done
done

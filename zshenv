VISUAL=emacs
path=(~/bin $path[@])
# alias emacs="emacs -nw"

NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"

export PATH="$PATH:./node_modules/.bin"

export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"


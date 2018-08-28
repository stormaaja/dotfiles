EDITOR=emacs
VISUAL="$EDITOR"
GIT_EDITOR="$EDITOR"
typeset -U path
path=(~/bin $path[@])
# export PATH=~/.npm-global/bin:/home/matti/programs/emsdk:/home/matti/programs/emsdk/clang/e1.37.35_64bit:/home/matti/programs/emsdk/node/8.9.1_64bit/bin:/home/matti/programs/emsdk/emscripten/1.37.35:$PATH
alias emacs="emacs -nw"

export NPM_PACKAGES="${HOME}/.npm-global"
export PATH="${HOME}/.npm-global/bin:${PATH}"
export MANPATH="${NPM_PACKAGES}/share/man:${MANPATH}"
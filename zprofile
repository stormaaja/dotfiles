EDITOR=vi
VISUAL="$EDITOR"
GIT_EDITOR="$EDITOR"
typeset -U path
path=(~/bin $path[@])
# export PATH=~/.npm-global/bin:/home/matti/programs/emsdk:/home/matti/programs/emsdk/clang/e1.37.35_64bit:/home/matti/programs/emsdk/node/8.9.1_64bit/bin:/home/matti/programs/emsdk/emscripten/1.37.35:$PATH
#alias emacs="emacs -nw"
alias vi="nvim"

# export NPM_PACKAGES="${HOME}/.npm-global"
# export PATH="${HOME}/.npm-global/bin:${PATH}"
# export MANPATH="${NPM_PACKAGES}/share/man:${MANPATH}"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export PATH="./node_modules/.bin:${PATH}"

VISUAL=emacs
path=(~/bin $path[@])
# alias emacs="emacs -nw"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# export PATH="./node_modules/.bin:${PATH}"

NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"

export PATH="$PATH:./node_modules/.bin"

export PATH="$PATH:${HOME}/programs/gcc-arm-none-eabi-8-2019-q3-update/bin"

export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

export PATH="${PATH}:${HOME}/programs/sonar-scanner-4.2.0.1873-macosx/bin"

export JAVA_HOME=`/usr/libexec/java_home -v 1.8`

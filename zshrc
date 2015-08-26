# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="ym"

# Red dots displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(zsh-syntax-highlighting)
# Settings
source $ZSH/oh-my-zsh.sh
unsetopt correct_all
DISABLE_AUTO_TITLE=true # disable tmux window auto-renaming

# Environment varibles
export EDITOR="vim"
export SHELL="zsh"

# Bindings
bindkey -v
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey '^R' history-incremental-pattern-search-backward
bindkey -M vicmd '^R' history-incremental-pattern-search-backward

# Functions and aliases
alias attach-main="tmux new -t 'main'" # attaches to main in a different window
alias session-main="bash $HOME/dotfiles/bin/session-main.sh"
alias spec="mocha -t 5000 -R spec"
alias ns="npm run-script"
alias vinstall="vim +BundleInstall +qall"
alias hog="hoogle -c -n 10 "

alias cabalspec="cabal test --show-details=always --test-option=--color"

function mkcd () {
    mkdir $1;
    cd $1;
}
alias gcc-allegro="gcc -L/usr/local/lib -lallegro -lallegro_main"

# vim + ack
function vack () {
    vim +"Ack $1" +"only"
}

function ep {
    emacsclient -t -s $(basename $(projectroot)) $@
}

function ep-start {
    projectroot &&
      emacs --daemon=$(basename $(projectroot)) $@
}

function ep-stop {
    projectroot &&
      emacsclient -t -s $(basename $(projectroot)) -e '(save-buffers-kill-emacs)'
}

# emacsclient GUI
alias egui="emacsclient -c -a emacs"


# Linux Stuff
if [[ $(uname) == 'Linux' ]]; then
    # Tmux fix for linux
    alias tmux="TERM=screen-256color-bce `which tmux`"
    # Ack alias
    alias ack=ack-grep
    # chromeos and ubuntu default boot
    alias chromeos-defaultboot="sudo cgpt add -i 6 -P 0 -S 1 /dev/sda"
    alias ubuntu-defaultboot="sudo cgpt add -i 6 -P 5 -S 1 /dev/sda"
# OSX stuff
elif [[ $(uname) == 'Darwin' ]]; then
    # UTF-8 support in daily use tools
    alias tmux="tmux -u"
    export LESSCHARSET=UTF-8
    # add npm bin to PATH
    export PATH=/usr/local/share/npm/bin:$PATH
    # add brew before the path
    export PATH=/usr/local/sbin:$PATH
    export PATH=/usr/local/bin:$PATH
    # add local node binaries before all paths
    export PATH="./node_modules/.bin:$PATH"
    # add gobrew to the path
    export PATH=$PATH:$HOME/.gobrew/bin
    alias ctags="`brew --prefix`/bin/ctags"

    fzf-autojump-widget() {
      cd $(
        autojump -s | ghead -n -7
        sort -n |
        egrep -oP '^[^\s]+\s+(\K.*)$' |
        fzf --reverse +s
      )
    }

    zle     -N   fzf-autojump-widget
    bindkey '^F' fzf-autojump-widget
    bindkey -s '^O' 'cd -\n'

    # Volume for OSX
    function vol () {
    alias love=/opt/homebrew-cask/Caskroom/love/0.9.1/love.app/Contents/MacOS/love
      osascript -e "set volume $1"
    }
    # Marks and jumping
    [[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh
    autoload -U compinit && compinit -u
    alias vim='mvim -v'
    alias irs='irssi'
    export DOCKER_HOST=tcp://192.168.59.103:2376
    export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm
    export DOCKER_TLS_VERIFY=1
fi

export PATH=$HOME/Library/Haskell/bin:$PATH # Add GHC path to PATH for scripting
export PATH=$HOME/.cabal/bin:$PATH          # Add Cabal path to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export GOPATH="/Users/yamadapc/program/g.toggl.com/toggl_api"
export GOROOT=`go env GOROOT`
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# Base16 Shell
BASE16_SCHEME="summerfruit"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# This loads DVM into a shell session.
if [ -s $HOME/.dvm/scripts/dvm ] ; then
	. $HOME/.dvm/scripts/dvm
fi

source ~/.profile
export FZF_TMUX=-1
source ~/.fzf.zsh

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# add ~/.bin to PATH
mkdir -p $HOME/.bin
export PATH=$HOME/.bin:$PATH

# setup NIX. We need to have the Yesemite branch from
# https://github.com/joelteon/nixpkgs.git it seems
# see http://www.chrisjr.org/posts/2014/12/22/nix-osx-haskell/#getting-started-with-nix-on-yosemite
#source ~/.nix-profile/etc/profile.d/nix.sh
#NIXDIR=~/nix
#export NIX_PATH=$NIXDIR/nixpkgs:nixpkgs=$NIXDIR/nixpkgs

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

# unregister broken GHC packages. Run this a few times to resolve dependency rot in installed packages.
# ghc-pkg-clean -f cabal/dev/packages*.conf also works.
function ghc-pkg-clean() {
  for p in `ghc-pkg check $* 2>&1  | grep problems | awk '{print $6}' | sed -e 's/:$//'`
  do
    echo unregistering $p; ghc-pkg $* unregister $p
  done
}

# remove all installed GHC/cabal packages, leaving ~/.cabal binaries and docs in place.
# When all else fails, use this to get out of dependency hell and start over.
function ghc-pkg-reset() {
  if [[ $(readlink -f /proc/$$/exe) =~ zsh ]]; then
    read 'ans?Erasing all your user ghc and cabal packages - are you sure (y/N)? '
  else # assume bash/bash compatible otherwise
    read -p 'Erasing all your user ghc and cabal packages - are you sure (y/N)? ' ans
  fi

  [[ x$ans =~ "xy" ]] && ( \
    echo 'erasing directories under ~/.ghc'; command rm -rf `find ~/.ghc/* -maxdepth 1 -type d`; \
    echo 'erasing ~/.cabal/lib'; command rm -rf ~/.cabal/lib; \
  )
}

alias cabalupgrades="cabal list --installed  | egrep -iv '(synopsis|homepage|license)'"

# added by travis gem
[ -f /Users/yamadapc/.travis/travis.sh ] && source /Users/yamadapc/.travis/travis.sh

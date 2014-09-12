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
plugins=(git zsh-syntax-highlighting, github)
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

function mkcd () {
    mkdir $1;
    cd $1;
}
alias gcc-allegro="gcc -L/usr/local/lib -lallegro -lallegro_main"

# vim + ack
function vack () {
    vim +"Ack $1" +"only"
}

# add ~/.bin to PATH
mkdir -p $HOME/.bin
export PATH=$HOME/.bin:$PATH

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
    alias tmux="tmux -u"
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
    alias less=$PAGER
    alias zless=$PAGER
    # Volume for OSX
    function vol () {
      osascript -e "set volume $1"
    }
    # Marks and jumping
    [[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh
    autoload -U compinit && compinit -u
    alias vim='mvim -v'
    alias irs='irssi'
fi

PATH=$HOME/Library/Haskell/bin:$PATH # Add GHC path to PATH for scripting
PATH=$HOME/.cabal/bin:$PATH          # Add Cabal path to PATH for scripting

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export GOPATH=~/program/golang/
export PATH=$GOPATH/bin:$PATH

# Base16 Shell
BASE16_SCHEME="default"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

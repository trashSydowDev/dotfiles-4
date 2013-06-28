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
plugins=(git)
# Settings
source $ZSH/oh-my-zsh.sh
unsetopt correct_all
# Customize to your needs...

# Bindings
bindkey -v
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey '^R' history-incremental-pattern-search-backward
bindkey -M vicmd '^R' history-incremental-pattern-search-backward

# Functions and aliases
function mkcd () {
	mkdir $1;
	cd $1;
}
alias gcc-allegro="gcc -L/usr/local/lib -lallegro -lallegro_main"

# Determine which OS zsh is running on
platform='unknown'
if [[ $(uname) == 'Linux' ]]; then
   platform='linux'
elif [[ $(uname) == 'Darwin' ]]; then
   platform='osx'
fi

# Linux Stuff
if [[ $platform == 'linux' ]]; then
    # Tmux fix for linux
    alias tmux="TERM=screen-256color-bce `which tmux`"
    # Ack alias
    alias ack=ack-grep
    # xcape for linux
    xcape
    # chromeos and ubuntu default boot
    alias chromeos-defaultboot="sudo cgpt add -i 6 -P 0 -S 1 /dev/sda"
    alias ubuntu-defaultboot="sudo cgpt add -i 6 -P 5 -S 1 /dev/sda"

# OSX stuff
elif [[ $platform == 'osx' ]]; then
    # Use vim pager
    export PAGER=~/.bin/vimpager
    alias ctags="`brew --prefix`/bin/ctags"
    alias less=$PAGER
    alias zless=$PAGER
    # Volume for OSX
    function vol () {
      osascript -e "set volume $1"
    }
    alias vim='mvim -v'
    alias irs='irssi'
fi

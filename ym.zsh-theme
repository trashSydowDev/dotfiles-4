# Mar 2013 ym

# Machine name.
function box_name {
    [ -f ~/.box-name ] && cat ~/.box-name || hostname -s
}

# Directory info.
local current_dir='${PWD/#$HOME/~}'

# Git info.
local git_info='$(git_prompt_info)'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}%{$reset_color%}(%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%})"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}x"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[magenta]%}o"

PROMPT="%{$terminfo[bold]$fg[blue]%}%{$reset_color%}\
%{$terminfo[bold]$fg[green]%}[${current_dir}]%{$reset_color%}\
%{$terminfo[bold]$fg[white]%}$ %{$reset_color%}"
RPROMPT="${git_info} %*"

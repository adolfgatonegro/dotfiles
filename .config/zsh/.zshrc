# ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀.zshrc
#⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀-------
#⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀Configuration file for ZSH.
#⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀
#⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀Nothing fancy, just some plugins, settings, and a
#⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀bunch of aliases that I rarely remember.
#⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀

[[ $- != *i* ]] && return

export TERM="xterm-kitty"
export EDITOR='nvim'
export VISUAL='nvim'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE
export HISTFILE=$XDG_CONFIG_HOME/zsh/zsh-hist
export HISTTIMEFORMAT="[%F %T]"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="cd *|ls *|mv *|cp *|rm * | docker rm * | docker rmi *"

setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt MENUCOMPLETE
setopt EXTENDEDGLOB
zle_highlight=("paste:none")

# Completion 
autoload -Uz compinit
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu select
zmodload zsh/complist

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^k" up-line-or-beginning-search
bindkey "^j" down-line-or-beginning-search

_comp_options+=(globdots)
compinit

# Colours
autoload -Uz colors && colors

# Source functions
source "$ZDOTDIR/zsh-functions"

# Prompt
autoload -Uz promptinit
promptinit

setopt PROMPT_SUBST

typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

preexec_functions+='preexec_update_git_vars'
precmd_functions+='precmd_update_git_vars'
chpwd_functions+='chpwd_update_git_vars'

NEWLINE=$'\n'
PROMPT=$'%{${fg[cyan]}%}%B%~%b$(prompt_git_info)%{${fg[default]}%}${NEWLINE}%F{001}%f '

# Old, basic prompt
# autoload -Uz vcs_info
# precmd() { vcs_info }
# zstyle ':vcs_info:git:*' formats ' %F{008}on %F{015} %B%F{004}%b'
# PROMPT=' %F{006}%B%1~%b${vcs_info_msg_0_}%b %F{001}❯%f '

zsh_source_file "zsh-aliases"
zsh_source_file "zsh-vim-mode"
zsh_source_file "zsh-functions"
# zsh_source_file "zsh-normie-mode"

zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"

fetch

# .zshrc - main config file for Z shell
# github.com/adolfgatonegro
# gatoneg.ro

[[ $- != *i* ]] && return

export TERM="xterm-kitty"
export EDITOR='vim'
export VISUAL='vim'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export HISTFILE=$XDG_CONFIG_HOME/zsh/zsh-hist
export HISTTIMEFORMAT="[%F %T]"
export HISTORY_IGNORE="(cd(| *)|ls(| *)|lf(| *)|mv(| *)|cp(| *)|rm(| *)|vim(| *)|.*)|link_handler(| *)|trem(|*)"
export ZSH_AUTOSUGGEST_STRATEGY=(completion)
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="(cd *|ls *|mv *|cp *|rm *)"
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt MENUCOMPLETE
setopt EXTENDEDGLOB
setopt AUTOCD
zle_highlight=("paste:none")

# Completion 
autoload -Uz compinit && compinit -d $XDG_CACHE_HOME/.zcompdump-$ZSH_VERSION
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu select
_comp_options+=(globdots)
zmodload zsh/complist

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
zle -N zsh_exit
bindkey "^k" up-line-or-beginning-search
bindkey "^j" down-line-or-beginning-search
bindkey "^D" zsh_exit

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

prompt_symbol=" "
PROMPT=$'%F{green}╭───[%F{white}%B%~%b%F{green}]$(prompt_git_info)\n%F{red}$prompt_symbol%f%F{cyan}%f '

zstyle ":history-search-multi-word" page-size "4"                      # Number of entries to show (default is $LINES/3)
zstyle ":history-search-multi-word" highlight-color "fg=yellow,bold"   # Color in which to highlight matched, searched text (default bg=17 on 256-color terminals)
zstyle ":plugin:history-search-multi-word" synhl "yes"                 # Whether to perform syntax highlighting (default true)
zstyle ":plugin:history-search-multi-word" active "bold"          # Effect on active history entry. Try: standout, bold, bg=blue (default underline)
zstyle ":plugin:history-search-multi-word" check-paths "yes"           # Whether to check paths for existence and mark with magenta (default true)
zstyle ":plugin:history-search-multi-word" clear-on-cancel "yes"        # Whether pressing Ctrl-C or ESC should clear entered query

zsh_source_file "zsh-aliases"
zsh_source_file "zsh-vim-mode"
zsh_source_file "zsh-functions"

zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zdharma-continuum/fast-syntax-highlighting"
zsh_add_plugin "zdharma-continuum/history-search-multi-word"
zsh_add_plugin "hlissner/zsh-autopair"

gato

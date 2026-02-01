#!/bin/zsh
# .zshrc
# gatoneg.ro
#
# Main configuration file for Z Shell.

# References
# https://zserge.com/posts/terminal/
# https://thevaluable.dev/zsh-completion-guide-examples/

# Enable Powerlevel10 instant prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Source aliases
if [[ -r "${XDG_CONFIG_HOME}/shell/aliases" ]]; then
  source "${XDG_CONFIG_HOME}/shell/aliases"
fi

# Source functions
if [[ -r "${XDG_CONFIG_HOME}/shell/functions" ]]; then
  source "${XDG_CONFIG_HOME}/shell/functions"
fi

# Shell history
HISTFILE="${ZDOTDIR}/history" # The path to the history file.
HISTSIZE=5000
SAVEHIST=5000
setopt INC_APPEND_HISTORY # Add to $HISTFILE incrementally.
setopt EXTENDED_HISTORY # Save command timestamp and duration.
setopt HIST_IGNORE_ALL_DUPS # Remove duplicated commands from history.
setopt HIST_IGNORE_SPACE # Ignore command lines starting with a space.
setopt CORRECT_ALL # Try to correct spelling mistakes of arguments in a line.

# Shell options
setopt autocd # Type the name of a dir to cd into it.
setopt auto_menu menu_complete # autocmp first menu match
setopt auto_param_slash # when a dir is completed, add a / instead of a trailing space
setopt no_case_glob no_case_match # make cmp case insensitive

# zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(completion history)

# Completions
zstyle ':completion:*' completer _complete _correct _approximate 
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu select

# Vi mode
zle-keymap-select () {
  zle reset-prompt
}
zle -N zle-keymap-select
zle-line-init () {
  zle -K viins
}
zle -N zle-line-init
bindkey -v

# Emacs bindings for vi mode
bindkey '\e[3~' delete-char
bindkey '^A'    beginning-of-line
bindkey '^E'    end-of-line
bindkey '^R'    history-incremental-pattern-search-backward

# Bindings for vi mode
bindkey -M viins '^?' backward-delete-char

# Bindings for complist module
zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'i' vi-insert
bindkey '^ ' autosuggest-accept

# Load zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

# Load plugins
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light hlissner/zsh-autopair

# Load powerlevel10k theme
zinit ice depth"1" # git clone depth
zinit light romkatv/powerlevel10k

# Enable modules
autoload -U compinit colors 
colors
compinit

# Source p10k
# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

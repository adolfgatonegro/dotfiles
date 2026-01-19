# ~~~ Path configuration ~~~
setopt extended_glob null_glob
# path=(
# 	$path
# 	$HOME/.bin
# 	$HOME/.local/bin
# )
# # Remove duplicate entries and non-existing directories
# typeset -U path
# path=($^path(N-/))
# export PATH

# ~~~ Environment variables ~~~
bindkey -v
bindkey "^R" history-incremental-search-backward
bindkey -M viins '^?' backward-delete-char

# Change cursor shape for different vi modes.
zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';; # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap
	echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# ~~~ History ~~~

HISTFILE="${ZDOTDIR:-$HOME/.config/zsh}/zsh_history" # The path to the history file.
HISTSIZE=5000
SAVEHIST=$HISTSIZE
HISTCONTROL=ignoreboth

# Opts
setopt append_history inc_append_history share_history # better history
setopt auto_menu menu_complete # autocmp first menu match
setopt autocd # type a dir to cd
setopt auto_param_slash # when a dir is completed, add a / instead of a trailing space
setopt no_case_glob no_case_match # make cmp case insensitive
setopt globdots # include dotfiles
setopt hist_ignore_all_dups # ignore duplicate commands
setopt extended_glob # match ~ # ^
setopt interactive_comments # allow comments in shell
unsetopt prompt_sp # don't autoclean blanklines

# ~~~ Prompt ~~~
PURE_PROMPT_SYMBOL="❯"
PURE_PROMPT_VICMD_SYMBOL=""

fpath+=($ZDOTDIR/plugins/pure)

autoload -U promptinit; promptinit
zstyle :prompt:pure:path color 'green'
zstyle :prompt:pure:prompt:success color 'cyan'
prompt pure

# ~~~ Sourcing ~~~
# Aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Functions
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/functions" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/functions"

source <(fzf --zsh)

# PLugin manager
source "${ZDOTDIR:-$HOME/.config/zsh}/zsh-plugins.zsh"

# Plugins
zsh_add_plugin "Aloxaf/fzf-tab"
zsh_add_plugin "zdharma-continuum/fast-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"

# ~~~ Completion ~~~
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu no
# zstyle ':completion:*' special-dirs true
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'

autoload -Uz compinit
compinit -u
autopair-init

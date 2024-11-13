# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set key timeout
KEYTIMEOUT=1

HISTFILE="${ZDOTDIR:-$HOME/.config/zsh}/zsh_history" # The path to the history file.
HISTSIZE=5000                        # The maximum number of events to save in the internal history.
SAVEHIST=$HISTSIZE                   # The maximum number of events to save in the history file.
HISTORY_IGNORE="(cd(| *)|ls(| *)|la(| *)|lh(| *)|ll(| *)|lf(| *)|mv(| *)|cp(| *)|rm(| *)|vim(| *)|nvim(| *)|helix(| *)|hx(| *)|.*)|linkhandler(| *)|trem(|*)|mpv(|*)|nb(|*)|umpv(|*)|builtin cd(|*)|ex(|*)|ff(|*)"

setopt BANG_HIST                     # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY              # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY            # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY                 # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST        # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS              # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS          # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS             # Do not display a line previously found.
setopt HIST_IGNORE_SPACE             # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS             # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS            # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY                   # Do not execute immediately upon history expansion.
setopt glob_dots

# zsh autosuggest
ZSH_AUTOSUGGEST_HISTORY_IGNORE="(cd *|ls *|mv *|cp *|rm *)"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion history)

# Set other options
setopt menucomplete
setopt extendedglob
setopt autocd

# Disable paste highlighting
zle_highlight=("paste:none")

# PLugin manager
source "${ZDOTDIR:-$HOME/.config/zsh}/zsh-plugins.zsh"

# Plugins
zsh_add_plugin "romkatv/powerlevel10k"
zsh_add_plugin "Aloxaf/fzf-tab"
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-completions"
zsh_add_plugin "zdharma-continuum/fast-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"

# Completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'

# Load completions
autoload -Uz compinit && compinit -d $XDG_CACHE_HOME/.zcompdump-$ZSH_VERSION

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.p10k.zsh" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.p10k.zsh"

# Keybindings
bindkey -v
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^ ' autosuggest-accept
bindkey "^?" backward-delete-char
bindkey -M vicmd '/' history-incremental-search-backward
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M viins '^p' history-search-backward
bindkey -M viins '^n' history-search-forward

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

# Exit with Ctrl+D
zsh_exit(){exit}
zle -N zsh_exit
bindkey "^D" zsh_exit

# Source aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Source functions
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/functions" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/functions"

# Shell integrations
eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"

autopair-init
gato

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set key timeout
KEYTIMEOUT=1

# Set history options
HISTSIZE=5000
HISTFILE="$ZDOTDIR/history"
SAVEHIST=$HISTSIZE
HISTDUP=erase
HISTORY_IGNORE="(cd(| *)|ls(| *)|la(| *)|lh(| *)|ll(| *)|lf(| *)|mv(| *)|cp(| *)|rm(| *)|vim(| *)|nvim(| *)|.*)|linkhandler(| *)|trem(|*)|conf(|*)|mpv(|*)|nb(|*)|umpv(|*)|builtin cd(|*)|ex(|*)|ff(|*)"
ZSH_AUTOSUGGEST_HISTORY_IGNORE="(cd *|ls *|mv *|cp *|rm *)"
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Set autosuggest strategy
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion history)

# Set other options
setopt menucomplete
setopt extendedglob
setopt autocd

# Disable paste highlighting
zle_highlight=("paste:none")

# Zinit
# Set zinit config directory
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download zinit if not present already
if [ ! -d "$ZINIT_HOME" ]; then
	mkdir -p "$(dirname $ZINIT_HOME)"
	git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source zinit
source "${ZINIT_HOME}/zinit.zsh"

# Add Powerlevel10K
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Plugins
zinit light Aloxaf/fzf-tab
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light hlissner/zsh-autopair

# Completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'

# Load completions
autoload -Uz compinit && compinit -d $XDG_CACHE_HOME/.zcompdump-$ZSH_VERSION
zinit cdreplay -q

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.p10k.zsh" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.p10k.zsh"

# Keybindings
bindkey -v
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^ ' autosuggest-accept
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

# Don't add failed commands to history file
zshaddhistory() {
	whence ${${(z)1}[1]} >| /dev/null || return 1
}

# Shell integrations
eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"

gato

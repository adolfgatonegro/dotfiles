# zsh config file
# gatoneg.ro

[[ $- != *i* ]] && return

export KEYTIMEOUT=1
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export HISTFILE="$ZDOTDIR/history"
export HISTTIMEFORMAT="[%F %T]"
export HISTORY_IGNORE="(cd(| *)|ls(| *)|lf(| *)|mv(| *)|cp(| *)|rm(| *)|vim(| *)|.*)|link_handler(| *)|trem(|*)"
export ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion history)
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

# Functions
# Install and source plugins
zsh_add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 
        source "$ZDOTDIR/plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        source "$ZDOTDIR/plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    else
        echo "Cloning missing plugins..."
		git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi
}

# cd up [1-9] times
.{1..9} (){ local d=.; repeat ${0:1} d+=/..; cd $d;}

# Extract archive files
ex (){
	if [ -f $1 ] ; then
		case $1 in
			*.tar.bz2)   tar xjf $1   ;;
			*.tar.gz)    tar xzf $1   ;;
			*.bz2)       bunzip2 $1   ;;
			*.rar)       unrar x $1   ;;
			*.gz)        gunzip $1    ;;
			*.tar)       tar xf $1    ;;
			*.tbz2)      tar xjf $1   ;;
			*.tgz)       tar xzf $1   ;;
			*.zip)       unzip $1     ;;
			*.Z)         uncompress $1;;
			*.7z)        7z x $1      ;;
			*.deb)       ar x $1      ;;
			*.tar.xz)    tar xf $1    ;;
			*.tar.zst)   tar xf $1    ;;
			*)           echo "'$1' cannot be extracted via ex()" ;;
		esac
	else
		echo "'$1' is not a valid file"
	fi
}

# Don't add failed commands to history
zshaddhistory() {
	whence ${${(z)1}[1]} >| /dev/null || return 1
}

# Git status on prompt adapted from https://gist.github.com/scelis/244215
update_current_git_vars(){
	unset __CURRENT_GIT_BRANCH
	unset __CURRENT_GIT_BRANCH_STATUS
	unset __CURRENT_GIT_BRANCH_IS_DIRTY

	local st="$(git status 2>/dev/null)"
	if [[ -n "$st" ]]; then
		local -a arr
		arr=(${(f)st})

		if [[ $arr[1] =~ 'Not currently on any branch.' ]]; then
			__CURRENT_GIT_BRANCH='no-branch'
		else
			__CURRENT_GIT_BRANCH="${arr[1][(w)4]}";
		fi

		if [[ $arr[2] =~ 'Your branch is' ]]; then
			if [[ $arr[2] =~ 'ahead' ]]; then
				__CURRENT_GIT_BRANCH_STATUS='ahead'
			elif [[ $arr[2] =~ 'diverged' ]]; then
				__CURRENT_GIT_BRANCH_STATUS='diverged'
			elif [[ $arr[2] =~ 'up to date' ]]; then
				__CURRENT_GIT_BRANCH_STATUS='uptodate'
			else
				__CURRENT_GIT_BRANCH_STATUS='behind'
			fi
		fi

		if [[ ! $st =~ 'nothing to commit' ]]; then
			__CURRENT_GIT_BRANCH_IS_DIRTY='1'
		fi
	fi
}

prompt_git_info(){
	if [ -n "$__CURRENT_GIT_BRANCH" ]; then
		local o="on "
		local b="$__CURRENT_GIT_BRANCH"
		case "$__CURRENT_GIT_BRANCH_STATUS" in
			ahead)
			s+=""
			;;
			diverged)
			s+="𝌎"
			;;
			uptodate)
			s+="🗸"
			;;
			behind)
			s+=""
			;;
		esac
		if [ -n "$__CURRENT_GIT_BRANCH_IS_DIRTY" ]; then
			local d="✗"
		fi
	 
		printf "%s" " %F{white}$o" " %F{blue}$b" " %F{green}%}$s" " %F{red}$d"
	fi
}

chpwd_update_git_vars(){
	update_current_git_vars
}

preexec_update_git_vars(){
	case "$1" in 
		git* | g[saclpdb]*)
		__EXECUTED_GIT_COMMAND=1
		;;
	esac
}

precmd_update_git_vars(){
	if [ -n "$__EXECUTED_GIT_COMMAND" ]; then
		update_current_git_vars
		unset __EXECUTED_GIT_COMMAND
	fi
}

zsh_exit(){exit}

precmd() {
	if [ -z "$_NEW_LINE_BEFORE_PROMPT" ]; then
		_NEW_LINE_BEFORE_PROMPT=1
	else
		print ""
	fi
}

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
PROMPT=$'%F{green}[%F{white}%B%~%b%F{green}]$(prompt_git_info)\n%F{red}$prompt_symbol%f%F{cyan}%f '

# set vim mode
bindkey -v

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# bindkey -M menuselect '^[[Z' vi-up-line-or-history
bindkey -v '^?' backward-delete-char
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^ ' autosuggest-accept

# Search backwards and forwards with a pattern
bindkey -M vicmd '/' history-incremental-search-backward
bindkey -M viins '^R' history-incremental-search-backward

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Load aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Add plugins
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zdharma-continuum/fast-syntax-highlighting"
zsh_add_plugin "zdharma-continuum/history-search-multi-word"
zsh_add_plugin "hlissner/zsh-autopair"

# Set up multi-word search history
zstyle ":history-search-multi-word" page-size "4"
zstyle ":history-search-multi-word" highlight-color "fg=yellow,bold"
zstyle ":plugin:history-search-multi-word" synhl "yes"
zstyle ":plugin:history-search-multi-word" active "bold"
zstyle ":plugin:history-search-multi-word" check-paths "yes"
zstyle ":plugin:history-search-multi-word" clear-on-cancel "yes"

gato

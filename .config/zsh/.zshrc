# zsh config file
# gatoneg.ro

[[ $- != *i* ]] && return

export KEYTIMEOUT=1
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export HISTFILE="$ZDOTDIR/history"
export HISTORY_IGNORE="(cd(| *)|ls(| *)|la(| *)|lh(| *)|ll(| *)|lf(| *)|mv(| *)|cp(| *)|rm(| *)|vim(| *)|nvim(| *)|.*)|linkhandler(| *)|trem(|*)|mcd(|*)|mpv(|*)|umpv(|*)|builtin cd(|*)|ex(|*)|ff(|*)|yt(|*)"
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

flacsplit(){
	[ $# != 2 ] && echo "flacsplit: convert single-file flac albums into tagged 320kpbs mp3 tracks\nUsage: flacsplit /path/to/cue /path/to/flac" && return ||
	if [ -e "$1" -a -e "$2" ] && expr "$1" : '.*\.cue$' > /dev/null && expr "$2" : '.*\.flac$' > /dev/null; then
		tmpdir=$(mktemp -d /tmp/flacsplit.XXXXXX.d)
		echo "Splitting FLAC file into tracks..."
		shnsplit -t "%n %t" -d "$tmpdir" -o "cust ext=mp3 ffmpeg -i - -ab 320k %f" -f "$1" "$2"
		echo "Tagging tracks..."
		cuetag.sh "$1" "$tmpdir"/*
		echo -n "\nDone! Importing to beets music library..." && \
		beet import "$tmpdir" && \
		echo "\nCleaning up..." && rm -r "$tmpdir"
	else
	  echo "Invalid arguments. Use .cue and .flac files as arguments, and make sure the path is correct."
	fi
}

# run gitsum on $HOME/repos, and $HOMe/.local/src if it exists
gitall(){
	if [ -d "$HOME/.local/src" ]; then
		gitsum $HOME/.local/src && echo " " && gitsum $HOME/repos
	else
		gitsum $HOME/repos
	fi
}

# Set xkeyboard options, remap caps-lock, and repeat rate
kbd() {
	setxkbmap -model pc104 -layout us -variant altgr-intl -option caps:escape
	[ -r $XDG_CONFIG_HOME/X11/Xmodmap ] && xmodmap $XDG_CONFIG_HOME/X11/Xmodmap
	xset r rate 250 100
}

# mkdir and cd into it
mcd () {
    if [ $# = 0 ]; then
        echo "Usage: mcd <directory>"
        return
    fi
    mkdir -p "$1" && cd "$1"
}

# Generate a password and copy it to the clipboard
passgen() {
    local size=${1:-24}
    cat /dev/random | tr -dc '[:graph:]' | head -c$size | xclip -selection clipboard
}

# Simple stopwatch function, press [RETURN] for lap times
sw () {
	now=$(date +%s)sec
	while true; do
		printf "%s\r" $(TZ=UTC date --date now-$now +%H:%M:%S.%N)
		sleep 0.025
	done
}

# convert the specified files to mp3 320kbps
mp3conv(){
	[ $# != 1 ] && echo "mp3conv: find files in a directory and covert them to 320kbps mp3\nUsage: tomp3 [extension], e.g. flac, wav, etc." && return ||
	tmpdir=$(mktemp -d /tmp/mp3conv.XXXXXX.d)
	find -name "*.$1" -exec sh -c 'ffmpeg -i "{}" -ab 320k "'$tmpdir'/${0/.'$1'}.mp3"' {} \; || \
	echo -n "\nDone! Importing to beets music library..." && \
	beet import "$tmpdir" && \
	echo "\nCleaning up..." && rm -r "$tmpdir"
	# echo -n "\nDelete temporary files? [Y/N] "
	# read -k1
	# case $REPLY in
	# 	[Yy]) echo "\nCleaning up..." && rm -r "$tmpdir";;
	# 	[Nn]) return;;
	# esac
}

dnxconv(){
	[ $# != 1 ] && echo "dnxconv: covert video to DNxHD video\nUsage: dnxconv [extension], e.g. avi, mp4, etc." && return ||
	mkdir dnxconv
	find -name "*.$1" -exec sh -c 'ffmpeg -i "{}" -c:v dnxhd -vf "scale=1920:1080,fps=24000/1001,format=yuv422p10le" -profile:v dnxhr_hqx -c:a pcm_s16le -ar 48000 -hide_banner "dnxconv/${0/.'$1'}.mxf"' {} \;
}

# Install and source zsh plugins
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

# Don't add failed commands to history file
zshaddhistory() {
	whence ${${(z)1}[1]} >| /dev/null || return 1
}

zsh_exit(){exit}

# Prompt
autoload -Uz promptinit
promptinit

setopt PROMPT_SUBST

prompt_symbol=" "
PROMPT=$'%F{green}[%F{white}%B%~%b%F{green}]\n%F{red}$prompt_symbol%f%F{cyan}%f '

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
zle-keymap-select () {
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
zsh_add_plugin "hlissner/zsh-autopair"

# fzf
[ -f "/usr/share/fzf/key-bindings.zsh" ] && source "/usr/share/fzf/key-bindings.zsh"
[ -f "/usr/share/fzf/completion.zsh" ] && source "/usr/share/fzf/completion.zsh"

_fzf_compgen_path() { fd --type f . "$1" } # set default path for completion
_fzf_compgen_dir() { fd --type d . "$1" }
bindkey "^[g" fzf-cd-widget

se() { fd -tf . ~/.local/bin | fzf --preview 'bat -p --color=always {}' --bind 'enter:become($EDITOR {})' ;} # edit a script from .local/bin
ce() { fd -tf -H -d 2 . ~/.config | fzf --preview 'bat -p --color=always {}' --bind 'enter:become($EDITOR {})' ;} # edit a config file
fo() { file=$(fd -tf . | fzf) && xdg-open "$file" ;} # xdg-open a file
ed() { fd -tf . ~ |  fzf --preview 'bat -p --color=always {}' --bind 'enter:become($EDITOR {})' ;} # xdg-open a file
ff() { fd --max-depth=8 -td . ~ | fzf --bind 'enter:become(lfpv {})' ;} # navigate to folder and open lf

eval "$(starship init zsh)"
gato

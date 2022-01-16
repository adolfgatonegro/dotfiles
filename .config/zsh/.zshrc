#
# ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
#⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀.zshrc
#⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀-------
#⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀Configuration file for ZSH.
#⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀
#⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀Nothing fancy, just some plugins, settings, and a
#⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀bunch of aliases that I rarely remember.
#⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀
#
# -----------------------------------------------------------------------------  

# Basic configuration
[[ $- != *i* ]] && return		# If not running interactively, don't do anything

export TERM="xterm-kitty" # proper terminal colours
export EDITOR='nvim' # neovim as editor
export VISUAL='nvim' # neovim-qt as visual editor
export MANPAGER="sh -c 'col -bx | bat -l man -p'" # use bat as manpager
export LESSHISTFILE=- # less doesn't need a history file, seriously
export HISTCONTROL=ignoreboth:erasedups # keep the zsh history clean
export HISTSIZE=1000000 # save a *ton* of commands for some reason
export SAVEHIST=$HISTSIZE # same value
export HISTFILE=$XDG_CONFIG_HOME/zsh/zsh_history # keep the history file inside .config/zsh
export HISTTIMEFORMAT="[%F %T]" # add timestamp to history

# export PATH=$HOME/.local/bin:$HOME/blog/bin:$PATH

setopt INC_APPEND_HISTORY # add stuff to history incrementally instead of waiting for exit
setopt EXTENDED_HISTORY # save command, timestamp, and duration of execution
setopt HIST_IGNORE_ALL_DUPS # removes duplicate commands

zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' insert-unambiguous true
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
autoload -Uz compinit
compinit

# Prompt
autoload -Uz promptinit
promptinit

autoload -Uz vcs_info	# load vcs info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats ' %F{008}on %F{015} %B%F{004}%b'	# format vcs_info_msg_0_
 
setopt PROMPT_SUBST		# setup prompt with git branch name
PROMPT=' %F{006}%B%1~%b${vcs_info_msg_0_}%b %F{001}❯%f '

source $ZDOTDIR/zaliases

# Source plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-you-should-use/you-should-use.plugin.zsh
source /usr/share/zsh/plugins/zsh-vi-mode/zsh-vi-mode.plugin.zsh

# zsh-vi-mode - configure cursor
ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BEAM
ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_BLOCK
ZVM_VISUAL_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BLOCK

# ex - file extractor
ex ()
{
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

cdl() {                 
	cd "$@" && la; 
}

zshaddhistory() {
	whence ${${(z)1}[1]} >| /dev/null || return 1
}

ufetch

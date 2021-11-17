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

export TERM="xterm-256color" # proper terminal colours
export EDITOR='nvim' # neovim as editor
export VISUAL='nvim' # neovim as visual editor
export MANPAGER="sh -c 'col -bx | bat -l man -p'" # use bat as manpager
export LESSHISTFILE=- # less doesn't need a history file, seriously
export HISTCONTROL=ignoreboth:erasedups # keep the zsh history clean
export HISTSIZE=1000000 # save a *ton* of commands for some reason
export SAVEHIST=$HISTSIZE # same value
export HISTFILE=$HOME/.config/zsh/.zsh_history # keep the history file inside .config/zsh
export HISTTIMEFORMAT="[%F %T]" # add timestamp to history

if [ -d "$HOME/.bin" ] ;		# add .bin and .local/bin to PATH
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

setopt INC_APPEND_HISTORY # add stuff to history incrementally instead of waiting for exit
setopt EXTENDED_HISTORY # save command, timestamp, and duration of execution
setopt HIST_IGNORE_ALL_DUPS # removes duplicate commands

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # case-insensitive autocompletion
autoload -Uz compinit && compinit # initialise autocompletion

# Prompt
autoload -Uz promptinit
promptinit

autoload -Uz vcs_info	# load vcs info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats ' %F{008}on %F{015} %B%F{004}%b'	# format vcs_info_msg_0_
 
setopt PROMPT_SUBST		# setup prompt with git branch name
PROMPT=' %F{006}%B%1~%b${vcs_info_msg_0_}%b %F{001}%f '

# echo -e -n "\x1b[\x33 q" # Set blinking cursor. zsh-vi-mode already covers that.

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


# Aliases
alias grep='grep --color=auto'		# add colour to grep
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias df='df -h'					# human-readable output for df

# Aliases - pacman
alias ch='checkupdates'				# check for updates

alias pac-s='sudo pacman -S'		# install package from repos
alias pac-u='sudo pacman -U'		# install package from local file
alias pac-ss='pacman -Ss'			# search for package
alias pac-qi='pacman -Qi'			# display information about local package
alias pac-si='pacman -Si'			# display info about package in repo
alias pac-rn='sudo pacman -Rn'		# remove package and dependencies
alias pac-rns='sudo pacman -Rns'	# remove package and all dependencies
alias paru-s='paru -S'				# install package from AUR
alias paru-ss='paru -Ss'			# search for AUR package
alias paru-qi='paru -Qi'			# display information about local AUR package
alias paru-rn='paru -Rn'			# remove package with paru
alias paru-si='paru -Si'			# display info about package in AUR

alias update='sudo pacman -Syu'	# sync repos and update
alias aurupdate='paru -Sua'			# update AUR packages

alias pac-cleanup='sudo pacman -Rns $(pacman -Qtdq)'	# remove orphans and leftover packages
alias pac-unlock="sudo rm /var/lib/pacman/db.lck" 		# unlock pacman database 

alias mirrors="sudo reflector --latest 20 --fastest 20 --number 10 --sort rate --verbose --save /etc/pacman.d/mirrorlist"

# Aliases - System configuration
alias grub-update="sudo grub-mkconfig -o /boot/grub/grub.cfg" # update grub configuration

# Aliases - Edit system config files
alias vlightdm="sudoedit /etc/lightdm/lightdm.conf"
alias vpacman="sudoedit /etc/pacman.conf"
alias vgrub="sudoedit /etc/default/grub"
alias vgrubcfg="sudoedit /boot/grub/grub.cfg"
alias vmirrorlist="sudoedit /etc/pacman.d/mirrorlist"
alias vfstab="sudoedit /etc/fstab"

# Aliases - Edit common config files
alias vqtile="$EDITOR ~/.config/qtile/config.py"
alias vpicom="$EDITOR ~/.config/picom/picom.conf"
alias vautostart="$EDITOR ~/.config/qtile/scripts/autostart.sh"
alias vsxhkd="$EDITOR ~/.config/sxhkd/sxhkdrc"
alias vkitty="$EDITOR ~/.config/kitty/kitty.conf"
alias vrofi="$EDITOR ~/.config/rofi/config.rasi"
alias vinitv="$EDITOR ~/.config/nvim/init.vim"
alias vbash="$EDITOR ~/.bashrc"
alias vzsh="$EDITOR ~/.zshrc"

# Aliases - Dotfiles repo
alias dots="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME"
alias dots-a="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME add"
alias dots-s="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME status"
alias dots-c="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME commit"
alias dots-p="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME push"

# Aliases - Commands and shortcuts
alias ls='exa -l --color=always --group-directories-first' # default listing
alias la='exa -al --color=always --group-directories-first'  # all files and dirs
alias lh='exa -al | egrep "^\."' # hidden files

alias cp="cp -i -v" 	# verbose cp with confirmation
alias mv='mv -i -v'		# verbose mv with confirmation
alias rm="rm -v"		# verbose rm

alias v='nvim' 		# faster neovim
alias _='sudo'		# faster sudo
alias q='exit'		# quick exit
alias cat='bat'		# a better cat
alias less='bat'	# a better less
alias vf="vifmrun"	# vifm with Überzug file previews

alias yta-aac="youtube-dl --extract-audio --audio-format aac "		# youtube-dl audio
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "

alias ytv-best="youtube-dl -f bestvideo+bestaudio "					# youtube-dl video

cdl() {                 
        cd "$@" && la; 
}

ufetch

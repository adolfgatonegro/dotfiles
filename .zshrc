#
#            /o   ooooo          
#         oooooo oooooooo+       
#      /.  o ooo oooo ooooo\     
#    oo    /oooo ooo    \           ZSH
#  .oo     ( ooo ooo+oooooo         .zshrc
#  ooo     ooooo&ooo   oooooo       ....................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo     
#       ooooooooooo&//ooo(       
#          ooooooooooo/         
#  
#  Just a zsh config. Based on the default ArcoLinux config
#  but slowly becoming its own thing. 👌

############
## EXPORT ##
############
export TERM="xterm-256color"	# Proper terminal colours, please
export EDITOR='nvim'
export VISUAL='nvim'
export HISTCONTROL=ignoreboth:erasedups
export HISTORY_IGNORE="(ls|cd|pwd|exit|q|cd -|cd ..|neofetch|dots|dotsa|dotss|dotsc|dotsp)"

export MANPAGER="sh -c 'col -bx | bat -l man -p'"	# Use bat as manpager


#######################
## ZSH CONFIGURATION ##
#######################

[[ $- != *i* ]] && return	# If not running interactively, don't do anything

# Set $PATH
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

###############
## OH-MY-ZSH ##
###############

export ZSH=/usr/share/oh-my-zsh/	# Path to oh-my-zsh
ZSH_THEME="gozilla"					# Set the theme

# Load plugins
source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-you-should-use/you-should-use.plugin.zsh

#############
## ALIASES ##
#############
 
# Colorize the grep command output for ease of use (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

#readable output
alias df='df -h'

#pacman unlock
alias unlock="sudo rm /var/lib/pacman/db.lck"
alias rmpacmanlock="sudo rm /var/lib/pacman/db.lck"

#arcolinux logout unlock
alias rmlogoutlock="sudo rm /tmp/arcologout.lock"

#free
alias free="free -mt"

#use all cores
alias uac="sh ~/.bin/main/000*"

#continue download
alias wget="wget -c"

#userlist
alias userlist="cut -d: -f1 /etc/passwd"

#ps
alias psa="ps auxf"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"

#grub update
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"

#add new fonts
alias update-fc='sudo fc-cache -fv'

#hardware info --short
alias hw="hwinfo --short"

#check vulnerabilities microcode
alias microcode='grep . /sys/devices/system/cpu/vulnerabilities/*'

#get fastest mirrors in your neighborhood
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 30 --number 10 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 30 --number 10 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 30 --number 10 --sort age --save /etc/pacman.d/mirrorlist"

#our experimental - best option for the moment
alias mirrorx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 5 --sort rate --protocol https --save /etc/pacman.d/mirrorlist"
alias mirrorxx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 20 --sort rate --protocol https --save /etc/pacman.d/mirrorlist"

#mounting the folder Public for exchange between host and guest on virtualbox
alias vbm="sudo /usr/local/bin/arcolinux-vbox-share"

#youtube-dl
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "

alias ytv-best="youtube-dl -f bestvideo+bestaudio "

#Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

#iso and version used to install ArcoLinux
alias iso="cat /etc/dev-rel | awk -F '=' '/ISO/ {print $2}'"


#search content with ripgrep
alias rg="rg --sort path"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

#gpg
#verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
alias fix-gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"

#receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
alias fix-gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
alias fix-key="[ -d ~/.gnupg ] || mkdir ~/.gnupg ; cp /etc/pacman.d/gnupg/gpg.conf ~/.gnupg/ ; echo 'done'"

#maintenance
alias big="expac -H M '%m\t%n' | sort -h | nl"
alias downgrada="sudo downgrade --ala-url https://ant.seedhost.eu/arcolinux/"

#systeminfo
alias probe="sudo -E hw-probe -all -upload"

#shutdown or reboot
alias ssn="sudo shutdown now"
alias sr="sudo reboot"

#update betterlockscreen images
alias bls="betterlockscreen -u /usr/share/backgrounds/arcolinux/"

#give the list of all installed desktops - xsessions desktops
alias xd="ls /usr/share/xsessions"

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
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

## Gato's aliases

alias vim='nvim'	# Neovim, please
alias pacss='pacman -Ss'		# Search for package
alias pacs='sudo pacman -S'	# Install package with pacman
alias pacu='sudo pacman -U'	# Install package from local file
alias pacqi='pacman -Qi'		# Display information about local package
alias pacsi='pacman -Si'		# Display info about package in repo
alias pacrn='sudo pacman -Rn'	# Remove package and dependencies
alias paruss='paru -Ss'			# Same, but for the AUR
alias parus='paru -S'			# Install package with paru
alias update='sudo pacman -Syu'	# Sync repos and update
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

# GitHub dotfiles repo
alias dots="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME"
alias dotsa="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME add"
alias dotss="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME status"
alias dotsc="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME commit -m"
alias dotsp="/usr/bin/git --git-dir=/gatonegro/Techno/dotfiles --work-tree=$HOME push"

# exa is a better ls than ls
alias ls='exa -l --color=always --group-directories-first' # default listing
alias la='exa -al --color=always --group-directories-first'  # all files and dirs
alias lh='exa -al | egrep "^\."' # hidden files

# Verbose output for file operation commands
alias cp="cp -i -v"
alias mv='mv -i -v'
alias rm="rm -v"

alias sound="pulsemixer"	# Sound settings from the terminal

# Edit common config files
alias vqtile="$EDITOR ~/.config/qtile/config.py"
alias vpicom="$EDITOR ~/.config/picom/picom.conf"
alias vautostart="$EDITOR ~/.config/qtile/scripts/autostart.sh"
alias vsxhkd="$EDITOR ~/.config/qtile/sxhkd/sxhkdrc"
alias vkitty="$EDITOR ~/.config/kitty/kitty.conf"
alias vrofi="$EDITOR ~/.config/rofi/config.rasi"
alias vinitv="$EDITOR ~/.config/nvim/init.vim"
alias vbash="$EDITOR ~/.bashrc"
alias vzsh="$EDITOR ~/.zshrc"

# Edit system config files (only if you really have to)
alias vlightdm="sudoedit /etc/lightdm/lightdm.conf"
alias vpacman="sudoedit /etc/pacman.conf"
alias vgrub="sudoedit /etc/default/grub"
alias vconfgrub="sudoedit /boot/grub/grub.cfg"
alias vmirrorlist="sudoedit /etc/pacman.d/mirrorlist"
alias vfstab="sudoedit /etc/fstab"

alias q="exit"										# Quick exit
alias cat="bat"										# A better cat
alias vifm="$HOME/.config/vifm/scripts/vifmrun"		# Vifm with Überzug file previews

# Set Vi mode
bindkey -v 

# Vi mode indicator for zsh prompt
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/[N]}/(main|viins)/}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

#########################
## END OF CONFIG FILE ###
#########################

neofetch	# Waste CPU cycles just to make the terminal look a bit nicer when launching

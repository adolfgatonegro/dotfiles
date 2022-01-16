export HOSTNAME=foxes
export VDPAU_DRIVER=nvidia
export QT_QPA_PLATFORMTHEME=qt5ct 

# Setting XDG system directories
export XDG_DATA_DIRS=/usr/local/share:/usr/share
export XDG_CONFIG_DIRS=/etc/xdg

# Setting XDG user directories
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

if [[ -d $HOME/.local/bin && -z $(echo $PATH | grep -o $HOME/.local/bin) ]]
then
    export PATH="${PATH}:$HOME/.local/bin"
fi

export XAUTHORITY=$XDG_CONFIG_HOME/X11/Xauthority
export XINITRC=$XDG_CONFIG_HOME/X11/xinitrc

export GTK2_RC_FILES=$XDG_CONFIG_HOME/gtk-2.0/gtkrc
export LESSHISTFILE=$XDG_CACHE_HOME/lesshst
export GNUPGHOME=$XDG_CONFIG_HOME/gnupg
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.lua" | so $MYVIMRC'
export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export CUDA_CACHE_PATH=$XDG_CACHE_HOME/nv
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export CARGO_HOME=$XDG_DATA_HOME/cargo

#! /bin/zsh

# Install and source zsh plugins
zsh_add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    PLUGIN_DIR="$ZDOTDIR/plugins/$PLUGIN_NAME"

    if [ -d "$PLUGIN_DIR" ]; then
		[ -r "$PLUGIN_DIR/$PLUGIN_NAME.plugin.zsh" ] && source "$PLUGIN_DIR/$PLUGIN_NAME.plugin.zsh"
		[ -r "$PLUGIN_DIR/$PLUGIN_NAME.zsh" ] && source "$PLUGIN_DIR/$PLUGIN_NAME.zsh"
		[ -r "$PLUGIN_DIR/$PLUGIN_NAME.zsh-theme" ] && source "$PLUGIN_DIR/$PLUGIN_NAME.zsh-theme"
    else
        echo "Cloning missing plugins..."
		git clone --depth=1 "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi
}

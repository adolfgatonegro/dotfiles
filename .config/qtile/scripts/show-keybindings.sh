#!/bin/bash
#
#            /o   ooooo
#         oooooo oooooooo+
#      /.  o ooo oooo ooooo\
#    oo    /oooo ooo    \           QTILE KEYBINDINGS HELPER
#  .oo     ( ooo ooo+oooooo         show-keybindings.sh
#  ooo     ooooo&ooo   oooooo       ........................
#  oooo    &oooooooo     oooo       Gatonegro
#   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
#     ooooooo    o        oo
#       ooooooooooo&//ooo(
#          ooooooooooo/
#
# Inspired by DistroTube's video: https://www.youtube.com/watch?v=WkXyXIs-ZMI,
# with some extra bash and yad stuff to make it look nice. Don't really need this
# thing, but it was a fun scripting exercise if nothing else.
#
# Requires Nerd Fonts' Ubuntu Mono to show the key symbols properly.

keybindings=$(sed -n '/KEYS_START/,/KEYS_END/p' ~/.config/qtile/keys.py | \
	grep -e 'Key(' \
		-e 'KEYS_GROUP' | \
	sed -e 's/^\s*[a-zA-Z]\w*(\[[a-z]\w*\]*,*\s*"*/	者  + /' \
		-e 's/+ shift/+ /' \
		-e 's/+ control/+ /' \
		-e 's/"*]*,\s*"/ + /' \
		-e 's/",\s[a-zA-Z].*=\s"/  		/' \
		-e 's/"),*//' \
		-e 's/\s*# KEYS_GROUP /\n\n	/' \
		-e 's/\s*#/\n/')

echo "$keybindings" | yad --text-info --title="Gato's Qtile Key Bindings" \
		--back='#222222' --fore='#dedede' --geometry=740x750 \
		--fontname="JetBrainsMono Nerd Font 8" --button="Got it:1" --close-on-unfocus

#!/bin/sh
# Raise window or start app by class, using gnome window-calls extension
# (https://github.com/ickyicky/window-calls)

alias wcall='gdbus call --session --dest org.gnome.Shell --object-path /org/gnome/Shell/Extensions/Windows'

function idof() {
    gdbus call --session --dest org.gnome.Shell \
          --object-path /org/gnome/Shell/Extensions/Windows \
          --method org.gnome.Shell.Extensions.Windows.List \
        | cut -f 2 -d"'" \
        | jq '[.[] | select(.wm_class == "'"$1"'").id][0]'
}

function raise() {
    if [ $# -ne 1 ]; then
        return 1
    fi
    gdbus call --session --dest org.gnome.Shell \
          --object-path /org/gnome/Shell/Extensions/Windows \
          --method org.gnome.Shell.Extensions.Windows.Activate "$1" > /dev/null
}

raise "$(idof "$1")" || exec $2

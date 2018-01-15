#!/bin/sh

# This script requires packages feh and imagemagick.
# Set DISPLAY=:0.0 before using this from somewhere else.

### Simple script

# feh --bg-fill --randomize --no-fehbg ~/usr/img/wall

### Complex script

# Variables
export DISPLAY=:0.0
DIR=$HOME/usr/img
INDEX=$DIR/wall.txt
IMG=$DIR/$(shuf -n 1 "$INDEX")
PORTRAIT=$(identify -format "%w %h" "$IMG" | awk '$1 < $2*1.2 { print 1 }')
CAPTIONFG=snow2
CAPTIONBG=none
CAPTIONSHADOW=gray15
POINTSIZE=13
WIDTH=1366
HEIGHT=768
OFFSET=+0-3

# Notify user
INFO=($(echo $IMG | awk -F/ '{print $7, $8, $9}'))
_CATEGORY=(${INFO[0]//-/ })
_ARTIST=(${INFO[1]//-/ })
_TITLE=${INFO[2]//-/ }
_TITLE=${_TITLE:0:$(( ${#_TITLE}-4 ))}
_TITLE=($(echo $_TITLE | tr -d "0-9()"))

# Capitalize
CATEGORY=${_CATEGORY[@]^}
ARTIST=${_ARTIST[@]^}
TITLE=${_TITLE[@]^}
MSG=$ARTIST" - "$TITLE

render_caption() {
    convert -background none -fill $CAPTIONFG -pointsize $POINTSIZE label:"$MSG" \
            \( +clone -background $CAPTIONSHADOW -shadow 450x2+0+0 \) \
            +swap -background $CAPTIONBG -layers merge +repage png:-
}

# Set wallpaper
if [ $PORTRAIT ]; then
     render_caption | convert -resize x${HEIGHT} $IMG \
                -gravity east -background gray8 -splice 1x0 \
                -gravity southeast - \
                -geometry $OFFSET -composite - | \
        feh --bg-tile --no-fehbg -
else
    render_caption | convert -resize ${WIDTH}x${HEIGHT}\! $IMG \
                -gravity south - \
                -geometry $OFFSET -composite - | \
        feh --bg-center --no-fehbg -
fi

#!/bin/sh
NAME=$(realpath $0)
DIR=$(dirname $NAME)
find $DIR -path $DIR/.git -prune -o -path $NAME -prune -o -type f -print |
while read line; do
    target=$(echo $line | sed s!$DIR/!$HOME/.!)
    install -Dm644 $line $target
    ln -sf $line $target
done

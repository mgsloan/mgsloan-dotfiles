#1/bin/bash -e

if [ -n HIDPI ]; then
    scrot ~/pics/screenshots-large/%F_%T.png --select --exec 'bash -c "convert $f -resize 50% ~/pics/screenshots/$n; google-chrome ~/pics/screenshots/$n"'
else
    scrot ~/pics/screenshots/%F_%T.png --select --exec 'google-chrome $f'
fi

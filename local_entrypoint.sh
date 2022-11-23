#!/usr/bin/env bash
# set -eo pipefail

echo "mkdir $MNT_DIR"
mkdir -p $MNT_DIR

chown shiny:shiny $MNT_DIR
ls -ld $MNT_DIR

rm -rf /srv/shiny-server/*
touch /srv/shiny-server/index.html
ln -s $MNT_DIR /srv/shiny-server/`basename $MNT_DIR`

# echo "Mount $BUCKET at $MNT_DIR"
# mount.gcsfuse -o uid=999,gid=999,implicit_dirs,allow_other $BUCKET $MNT_DIR
# echo "Done"

ln -s /app/* $MNT_DIR

echo "Start shiny-server"
sudo -u shiny shiny-server 
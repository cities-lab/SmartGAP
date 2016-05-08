#! /bin/bash

PWD="`dirname \"$0\"`"
cd "${PWD}/scripts/gui"
Rscript main.r

read -sn 1 -p "*** Press Any key to Continue ***"


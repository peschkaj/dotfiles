#!/usr/bin/env bash
#
#   Use rofi to select an entry from gpaste
#
#   Requirements:
#     rofi, gpaste
#

line=`gpaste-client --oneline | \
      rofi -dmenu -i -p gpaste -kb-custom-1 Ctrl+x $@`
exit_code=$?

index=`echo $line | cut -d ':' -f1`
if ! [[ $index =~ ^[0-9]+$ ]]; then
    exit 0
fi

if [ $exit_code == 0 ]; then
    gpaste-client select $index
elif [ $exit_code == 10 ]; then
    gpaste-client delete $index
fi

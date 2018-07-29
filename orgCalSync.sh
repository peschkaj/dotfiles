#!/bin/bash

# customize these
WGET=/usr/local/bin/wget
ICS2ORG=/usr/local/bin/ical2orgpy
ICSFILE=/Users/jeremiah/Documents/org/calsync/jeremiahpeschka.ics
ORGFILE=/Users/jeremiah/Documents/org/calsync/jeremiahpeschka-cal.org
URL=https://calendar.google.com/calendar/ical/jeremiah.peschka%40gmail.com/private-a1288e9305374ef03e7c555a8074ad88/basic.ics

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG $ICSFILE $ORGFILE
rm -f $ICSFILE

echo '#+CATEGORY: personal' | cat - $ORGFILE > temp && mv temp $ORGFILE

ICSFILE=/Users/jeremiah/Documents/org/calsync/legitbiz.ics
ORGFILE=/Users/jeremiah/Documents/org/calsync/legitbiz-cal.org
URL=https://user.fm/calendar/v1-9df9b444-779f-4e00-b9a6-30f8bf1fcbb4/Calendar.ics

$WGET -O $ICSFILE $URL
$ICS2ORG $ICSFILE $ORGFILE
rm -f $ICSFILE

echo '#+CATEGORY: legitbiz' | cat - $ORGFILE > temp && mv temp $ORGFILE

ICSFILE=/Users/jeremiah/Documents/org/calsync/jpeschka.ics
ORGFILE=/Users/jeremiah/Documents/org/calsync/jpeschka-cal.org
URL=https://calendar.google.com/calendar/ical/jpeschka%40pdx.edu/private-4d103f01e3d26f17c5a520f4dcbf0dd9/basic.ics

$WGET -O $ICSFILE $URL
$ICS2ORG $ICSFILE $ORGFILE
rm -f $ICSFILE

echo '#+CATEGORY: pdx.edu' | cat - $ORGFILE > temp && mv temp $ORGFILE

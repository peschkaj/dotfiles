#!env zsh

if [[ -a /var/run/reboot-required ]] then
   print "Yes, you should reboot";
   else
       print "Don't bother with a reboot";
fi

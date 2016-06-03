# dotfiles!

## /etc/sources.list

```
deb http://apt.insynchq.com/ubuntu xenial non-free contrib
```

## PPAs

```
apt-add-repository ppa:numix/ppa
apt-add-repository ppa:nilarimogard/webupd8
add-apt-repository ppa:ubuntu-mozilla-daily/firefox-aurora
```

## Packages to install

* build-essential
* nvidia-361
* prime-indicator
* bbswitch
* bbswitch-dkms
* primus
* powertop
* cpufrequtils
* emacs
* git
* htop
* exfat-fuse
* exfat-fuse-utils
* insync
* zsh
* arc-theme
* paper-icon-theme
* numix-icon-theme
* numix-icon-theme-circle
* acpid
* tlp
* krita
* gnome-tweak-tool
* unity-tweak-tool

## bumblebee configuration

Before venturing down this route, it may be possible to simply install bumblebee and have everything work just fine. Doublecheck the status of Issue #759: [Bumblebee not working in Ubuntu 16.04](https://github.com/Bumblebee-Project/Bumblebee/issues/759#issuecomment-222922338)

To get bumblebee (graphics switching) working correctly, follow the instructions at [Nvidia with Bumblebee installation for 16.04](http://askubuntu.com/a/749724/285038).

Supporting information can be found at [Bumblebee on a Lenovo T440p [NVidia GT 730M] with XUbuntu/Ubuntu 16.04 LTS](http://lenovolinux.blogspot.com.au/2016/05/bumblebee-on-lenovo-t440p-nvidia-gt.html)

Obviously, neither of the previous posts are necessary if the system in question only has one graphics card.

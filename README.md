# dotfiles!

## /etc/apt/sources.list

```
deb http://apt.insynchq.com/ubuntu xenial non-free contrib
```

## PPAs

```
apt-add-repository ppa:numix/ppa
apt-add-repository ppa:nilarimogard/webupd8
add-apt-repository ppa:moka/daily
add-apt-repository ppa:leolik/leolik
apt-add-repository ppa:eosrei/fonts
```

Add the [paper icon repository](https://snwh.org/paper/download) 

And then the docker configuration:
```
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
```

## Packages to install

* notifyosdconfig
* htop
* exfat-fuse
* exfat-fuse-utils
* insync
* zsh
* paper-icon-theme
* acpid
* gnome-tweak-tool
* unity-tweak-tool
* moka-icon-theme
* fonts-emojione-svginot

### For a Laptop

* tlp
* cpufrequtils
* powertop

### Clone the flatabulous theme

```
cd ~
mkdir .themes
git clone git@github.com:anmoljagetia/Flatabulous.git
```

### Install the Source Code Pro font

```
#!/bin/sh

echo "installing fonts at $PWD to ~/.fonts/"
mkdir -p ~/.fonts/adobe-fonts/source-code-pro
git clone --depth 1 --branch release https://github.com/adobe-fonts/source-code-pro.git ~/.fonts/adobe-fonts/source-code-pro
# find ~/.fonts/ -iname '*.ttf' -exec echo \{\} \;
fc-cache -f -v ~/.fonts/adobe-fonts/source-code-pro
echo "finished installing"
```

### VM Tools

* remmina
* qemu-kvm
* docker-engine
* aufs-tools
* linux-image-extra (may require a specific kernel version)
* virt-manager

### Development Tools

* markdown
* libssl-dev
* build-essential
* gdb
* cmake
* cmake-extras
* bless
* clang
* clang-3.8-doc
* libclang-3.8-dev
* llvm
* llvm-dev
* llvm-3.8-doc
* clang-format
* emacs
* exuberant-ctags
* git
* valgrind
* valgrind-dbg
* lldb
* lldb-3.8-dev
* libboost-all-dev
* racket racket-common racket-doc
* guile-2.0
* lua5.3 liblua5.3-dev
* mit-scheme
* global
 
**rtags**
```
# Create a valid symlink for llvm-config
sudo ln -s /usr/bin/llvm-config-3.8 /usr/local/bin/llvm-config

cd src
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
mkdir build
cd build
LIBCLANG_LLVM_CONFIG_EXECUTABLE=/usr/local/bin/llvm-config cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DRTAGS_NO_BUILD_CLANG=1 ..
make
sudo make install
```


### Graphics Drivers

* prime-indicator
* bbswitch
* bbswitch-dkms
* primus
* nvidia-361 - change this to the current nvida driver

## Install oh-my-zsh

```
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
mv .zshrc .zshrc.old
ln -s ~/src/peschkaj/dotfiles/.zshrc .zshrc
```

## Install spacemacs

```
cd ~
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s ~/src/peschkaj/dotfiles/.spacemacs .spacemacs
rm -rf ~/.emacs.d/private/
ln -s /home/jeremiah/src/peschkaj/dotfiles/spacemacs-private ~/.emacs.d/private
```



## Powerline Fonts

Install powerline fonts from [the github repository](https://github.com/powerline/fonts)

```
mkdir -p ~/src/powerline
cd ~/src/powerline
git clone https://github.com/powerline/fonts
cd fonts
./install.sh
```


## Global `.gitignore`

This doesn't work if it's configured in an include file, don't know why, but hey... I have a workaround.

```
ln -s ~/src/peschkaj/dotfiles/GIT_IGNORE ~/.gitignore_global
git config --global core.excludesfile ~/.gitignore_global
```

## Graphics

### NVidia Graphics Driver PPAs

Add the graphics driver PPA:

```
sudo apt-add-repository ppa:graphics-drivers/ppa
```

Check NVidia to see what the most recent driver is and install that.

### bumblebee configuration (optional)

Before venturing down this route, it may be possible to simply install bumblebee and have everything work just fine. Doublecheck the status of Issue #759: [Bumblebee not working in Ubuntu 16.04](https://github.com/Bumblebee-Project/Bumblebee/issues/759#issuecomment-222922338)

To get bumblebee (graphics switching) working correctly, follow the instructions at [Nvidia with Bumblebee installation for 16.04](http://askubuntu.com/a/749724/285038).

Supporting information can be found at [Bumblebee on a Lenovo T440p [NVidia GT 730M] with XUbuntu/Ubuntu 16.04 LTS](http://lenovolinux.blogspot.com.au/2016/05/bumblebee-on-lenovo-t440p-nvidia-gt.html)

Obviously, neither of the previous posts are necessary if the system in question only has one graphics card.

## Theme Tweaks

To get firefox to display correctly, follow the instructions in [Fixes/Tweaks For Best Dark Theme Functionality.](Fixes/Tweaks For Best Dark Theme Functionality.) - but only if using a full dark theme (e.g. Arc Dark).

## Removed Items

* ~~arc-theme (see [Install package arc-theme](http://software.opensuse.org/download.html?project=home%3AHorst3180&package=arc-theme) for more details)~~
* ~~numix-icon-theme~~
* ~~numix-icon-theme-circle~~

# dotfiles!

## Initial Set Up

``` shell
sudo apt install git
mkdir -p ~/src/peschkaj
cd ~/src/peschkaj
git clone git@github.com:peschkaj/dotfiles.git

ln -s ~/.zshrc ~/src/peschkaj/dotfiles/.zshrc
```

### Prepare for spacemacs

**N.B.** First emacs launch is going to take _forever_. If any of the ELPA repositories are down, this launch will fail until they're available.

```
cd ~
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s ~/src/peschkaj/dotfiles/.spacemacs .spacemacs
rm -rf ~/.emacs.d/private/
ln -s /home/jeremiah/src/peschkaj/dotfiles/spacemacs-private ~/.emacs.d/private

sudo cp ~/src/peschkaj/dotfiles/emc.sh /usr/local/bin/emc.sh
```

## /etc/apt/sources.list

``` shell
cd ~
wget -qO - https://d2t3ff60b2tol4.cloudfront.net/services@insynchq.com.gpg.key \
| sudo apt-key add -
sudo echo "\n\n\ndeb http://apt.insynchq.com/ubuntu xenial non-free contrib" >> /etc/apt/sources.list
```

## PPAs

``` shell
sudo add-apt-repository ppa:numix/ppa
sudo add-apt-repository ppa:nilarimogard/webupd8
sudo add-apt-repository ppa:moka/daily
sudo add-apt-repository ppa:leolik/leolik
sudo add-apt-repository ppa:eosrei/fonts
sudo add-apt-repository ppa:noobslab/icons
sudo add-apt-repository ppa:leolik/leolik
sudo add-apt-repository ppa:snwh/pulp
sudo apt-add-repository ppa:graphics-drivers/ppa

sudo apt update && sudo apt upgrade
```

Add the [paper icon repository](https://snwh.org/paper/download)

And then the docker configuration:
``` shell
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
```

## Packages to installed

``` shell
sudo apt install notifyosdconfig \
                 htop \
                 exfat-fuse exfat-fuse-utils \
                 insync \
                 zsh \
                 paper-icon-theme paper-gtk-theme paper-cursor-theme \
                 moka-icon-theme \
                 acpid \
                 gnome-tweak-tool unity-tweak-tool \
                 fonts-emojione-svginot
```

### For a Laptop

``` shell
sudo apt install tlp \
                 cpufrequtils \
                 powertop
```

### Clone the flatabulous theme

``` shell
cd ~
mkdir .themes
git clone git@github.com:anmoljagetia/Flatabulous.git
```

### Install the Source Code Pro font

``` shell
#!/bin/sh

echo "installing fonts at $PWD to ~/.fonts/"
mkdir -p ~/.fonts/adobe-fonts/source-code-pro
git clone --depth 1 --branch release https://github.com/adobe-fonts/source-code-pro.git ~/.fonts/adobe-fonts/source-code-pro
# find ~/.fonts/ -iname '*.ttf' -exec echo \{\} \;
fc-cache -f -v ~/.fonts/adobe-fonts/source-code-pro
echo "finished installing"
```

### Powerline Fonts

``` shell
mkdir -p ~/src/powerline/
cd ~/src/powerline
git clone https://github.com/powerline/fonts
cd fonts
./install.sh
```

### VM Tools

``` shell
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
```

Now install VM tooling:

``` shell
sudo apt install remmina \
                 qemu-kvm \
                 docker-engine \
                 aufs \
                 virt-manager
```

### Development Tools

``` shell
sudo apt install markdown libssl-dev gdb \
                 cmake cmake-extras bless \
                 clang llvm llvm-dev lldb libclang-dev clang-format \
                 clang-3.8-doc llvm-3.8-doc lldb lldb-3.8-dev \
                 zsh-doc \
                 exuberant-ctags valgrind valgrind-dbg libboost-all-dev \
                 racket racket-common racket-doc \
                 guile-2.0 lua5.3 liblua5.3-dev \
                 mit-scheme global \
                 emacs-lucid ncurses-term \
                 gnutls-bin libgnutls-dev
```

After installing emacs, edit `/usr/share/applications/emacs24-lucid.application` to point to the `/usr/local/bin/emc.sh` script.

**rtags**

```
cd ~/src
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
mkdir build
cd build
LIBCLANG_LLVM_CONFIG_EXECUTABLE=/usr/local/bin/llvm-config cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DRTAGS_NO_BUILD_CLANG=1 ..
make
sudo make install
```

Create a user daemon according to [Integration with `systemd`](https://github.com/Andersbakken/rtags#integration-with-systemd-gnu-linux)

**Rust**

```
curl https://sh.rustup.rs -sSf | sh
mkdir -p ~/src/rust-lang
cd ~/src/rust-lang
git clone git@github.com:rust-lang/rust.git

rustup default nightly
```

### Graphics Drivers

``` shell
sudo apt install prime-indicator \
                 bbswitch \
                 bbswitch-dkms \
                 primus
```

**Manually** add the NVidia repo and select the current NVidia driver.

## Install oh-my-zsh

```
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
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

cat << EOF > ~/.gitconfig
[include]
    path = /home/jeremiah/src/peschkaj/dotfiles/git/gitconfig
[include]
    path = /home/jeremiah/src/peschkaj/dotfiles/git/github
[include]
    path = /home/jeremiah/src/peschkaj/dotfiles/git/linux

[core]
    excludesfile = /home/jeremiah/.gitignore_global

EOF
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

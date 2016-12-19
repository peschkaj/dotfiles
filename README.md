# dotfiles!

## Initial Set Up

``` shell
sudo apt install zsh

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

sudo apt install git
mkdir -p ~/src/peschkaj
cd ~/src/peschkaj
git clone git@github.com:peschkaj/dotfiles.git

rm .zshrc .zshenv

ln -s ~/src/peschkaj/dotfiles/.zshrc ~/.zshrc
ln -s ~/src/peschkaj/dotfiles/.zshenv ~/.zshenv
```

Install etckeeper

``` shell
sudo apt install etckeeper
```

Set up `journald` to persist across restarts:

``` shell
# Edit /etc/systemd/journald.conf
# Change the Storage line to:
Storage=persistent
```

After this is complete, make sure to `sudo etckeeper commit "Setting journald storage to persistent"`

### Prepare for spacemacs

**N.B.** First emacs launch is going to take _forever_. If any of the ELPA repositories are down, this launch will fail until they're available.

```
cd ~
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s ~/src/peschkaj/dotfiles/.spacemacs .spacemacs
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/irony-mode ~/.emacs.d/private/irony-mode
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/rtags ~/.emacs.d/private/rtags
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/snippets ~/.emacs.d/private/snippets

sudo cp ~/src/peschkaj/dotfiles/emc.sh /usr/local/bin/emc.sh
```

## /etc/apt/sources.list

``` shell
cd ~
wget -qO - https://d2t3ff60b2tol4.cloudfront.net/services@insynchq.com.gpg.key \
| sudo apt-key add -
echo "deb http://apt.insynchq.com/ubuntu xenial non-free contrib" | sudo tee /etc/apt/sources.list.d/insync.list
```

## PPAs

``` shell
sudo add-apt-repository -y ppa:numix/ppa
sudo add-apt-repository -y ppa:nilarimogard/webupd8
sudo add-apt-repository -y ppa:moka/daily
sudo add-apt-repository -y ppa:leolik/leolik
sudo add-apt-repository -y ppa:eosrei/fonts
sudo add-apt-repository -y ppa:noobslab/icons
sudo add-apt-repository -y ppa:leolik/leolik
sudo add-apt-repository -y ppa:snwh/pulp
sudo apt-add-repository -y ppa:graphics-drivers/ppa

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
                 exfat-fuse exfat-utils \
                 insync \
                 paper-icon-theme paper-gtk-theme paper-cursor-theme \
                 moka-icon-theme \
                 acpid \
                 gnome-tweak-tool unity-tweak-tool
```

Install [replacement color emoji fonts](http://www.omgubuntu.co.uk/2016/08/enable-color-emoji-linux-google-chrome-noto)

Copy`[fonts.conf]()`:

``` shell
cp ~/src/peschkaj/dotfiles/fonts.conf ~/.config/fontconfig/fonts.conf
fc-cache -f -v
```


### For a Laptop

``` shell
sudo apt install tlp \
                 cpufrequtils \
                 powertop
```

## Fonts & Themes

### Clone the flatabulous theme

``` shell
cd ~
mkdir .themes
cd .themes
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
## Tools

### VM Tools

Install VM tooling:

``` shell
sudo apt install remmina \
                 qemu-kvm \
                 docker-engine \
                 aufs-tools \
                 virt-manager
```

Once Docker is up and running, consult [docker.md]() to configure Docker completely and to test the installation.

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
                 emacs24-lucid ncurses-term \
                 gnutls-bin libgnutls-dev
```

After installing emacs, edit `/usr/share/applications/emacs24-lucid.desktop` to point to the `/usr/local/bin/emc.sh` script.

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

It may not be necessary to use the `LIBCLANG_LLVM_CONFIG_EXECUTABLE`, depending on how everything is installed by packages.

Create a user daemon according to [Integration with `systemd`](https://github.com/Andersbakken/rtags#integration-with-systemd-gnu-linux)

**Rust**

```
curl https://sh.rustup.rs -sSf | sh
mkdir -p ~/src/rust-lang
cd ~/src/rust-lang
git clone git@github.com:rust-lang/rust.git

rustup default nightly
```

### Global `.gitignore` set up

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


## Documents & Pictures

After insync has brought down the root folder structure...

``` shell
cd ~
rm -rf Documents
ln -s ~/.insync/Documents Documents
rm -rf Pictures
ln -s ~/.insync/Pictures Pictures
```

## Graphics

### NVidia Graphics Driver PPAs

Check NVidia to see what the most recent driver is and install that.

Can also run `apt search --names-only nvidia` and scan the list.

As of 2016-08-17, the current nvidia driver is `nvidia-370`.

This should install bumblebee and friends, but if it doesn't:

``` shell
sudo apt install prime-indicator \
                 bbswitch \
                 bbswitch-dkms \
                 primus
```

### bumblebee configuration (optional)

Before venturing down this route, it may be possible to simply install bumblebee and have everything work just fine. Doublecheck the status of Issue #759: [Bumblebee not working in Ubuntu 16.04](https://github.com/Bumblebee-Project/Bumblebee/issues/759#issuecomment-222922338)

To get bumblebee (graphics switching) working correctly, follow the instructions at [Nvidia with Bumblebee installation for 16.04](http://askubuntu.com/a/749724/285038).

Supporting information can be found at [Bumblebee on a Lenovo T440p NVidia GT 730M with XUbuntu/Ubuntu 16.04 LTS](http://lenovolinux.blogspot.com.au/2016/05/bumblebee-on-lenovo-t440p-nvidia-gt.html)

Obviously, neither of the previous posts are necessary if the system in question only has one graphics card.

## Theme Tweaks

To get firefox to display correctly, follow the instructions in [Fixes/Tweaks For Best Dark Theme Functionality.](Fixes/Tweaks For Best Dark Theme Functionality.) - but only if using a full dark theme (e.g. Arc Dark).

## Removed Items

* ~~arc-theme (see [Install package arc-theme](http://software.opensuse.org/download.html?project=home%3AHorst3180&package=arc-theme) for more details)~~
* ~~numix-icon-theme~~
* ~~numix-icon-theme-circle~~

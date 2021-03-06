# dotfiles!

## Purpose of this Repository

This repo serves two purposes. 

**One** To allow me to rapidly set up a Linux workstation with the tools that I prefer to use. This document describes my installation process and it is a work in progress. If you find anything wrong, open an issue.

**Two** To share the customizations that I've made to my configuration and use that to help others.

## Initial Set Up

Installs [ZSH](https://en.wikipedia.org/wiki/Z_shell), [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh), and clones this repository as well as my secret repo of secret stuff.

``` shell
sudo apt install zsh git curl

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

mkdir -p ~/src/peschkaj
cd ~/src/peschkaj
git clone git@github.com:peschkaj/dotfiles.git
git clone git@github.com:peschkaj/seekrets.git

rm .zshrc .zshenv

ln -s ~/src/peschkaj/dotfiles/.zshrc ~/.zshrc
ln -s ~/src/peschkaj/dotfiles/.zshenv ~/.zshenv
```

Adds the [`git-open`](https://github.com/paulirish/git-open) ZSH plugin. This plugin makes it easy to open a git repo's website from the command line, but in the browser of your choice!

``` shell
cd ~/.oh-my-zsh/custom/plugins
git clone git@github.com:paulirish/git-open.git
# Reload the shell
. ~/.zshrc
```

Install [etckeeper](https://joeyh.name/code/etckeeper/) to make it easier to see the history of changes to configuration files on a given system.

``` shell
sudo apt install etckeeper
```

Set up `journald` to persist across restarts. By default, `journald` rotates out the journal with each reboot. Changing this setting makes it easier to troubleshoot a problem that you had to reboot to recover from.

``` shell
# Edit /etc/systemd/journald.conf
# Change the Storage line to:
Storage=persistent
```

After this is complete, make sure to `sudo etckeeper commit "Setting journald storage to persistent"`

### Set up RAID (if multiple drives are present)

My system is set up using [Btrfs](https://en.wikipedia.org/wiki/Btrfs) instead of ext4. Btrfs is intended to provide state of the art features like storage pooling, copy on write, and easy administration.

The desktop currently has two drives in RAID 1 (`sda` and `sdc`) for the system:

``` shell
sudo btrfs device add -f /dev/sdc /
sudo btrfs balance start -dconvert=raid1 -mconvert=raid1 /
```

The desktop also has a four volume RAID 10 with three mount points. These commands won't work for you. In fact, they're not even educational. This is just how I set it up on these drives which have been formatted since the dawn of time.

``` shell
sudo mkdir -p /opt/vm
sudo mkdir -p /opt/iso
sudo mkdir -p /opt/docker

echo " 
# 4 volume RAID 10
UUID=77328915-e420-47f4-8e00-26c7ac5a0134 /opt/vm          btrfs   defaults,ssd,discard,subvolid=258  0 0
UUID=77328915-e420-47f4-8e00-26c7ac5a0134 /opt/iso          btrfs   defaults,ssd,discard,subvolid=259  0 0
UUID=77328915-e420-47f4-8e00-26c7ac5a0134 /opt/docker          btrfs   defaults,ssd,discard,subvolid=260  0 0
" | sudo tee --append /etc/fstab


sudo mount -a
```


## /etc/apt/sources.list

### Insync

[Insync](https://www.insynchq.com/) is a cross platform Google Drive client that supports multiple user accounts as well as automagic document conversion. 

``` shell
cd ~
wget -qO - https://d2t3ff60b2tol4.cloudfront.net/services@insynchq.com.gpg.key \
| sudo apt-key add -
echo "deb http://apt.insynchq.com/ubuntu xenial non-free contrib" | sudo tee /etc/apt/sources.list.d/insync.list
```

### Docker

It's best to just pull Docker from their own repositories. 

``` shell
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
```

Normally you'd have to run an `apt update` here, but the next step is to add a bunch more apt repositories.

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

## Packages to install

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

Copy [`fonts.conf`](fonts.conf):

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

[Flatabulous](https://github.com/anmoljagetia/Flatabulous) is a flat theme for Unity

``` shell
cd ~
mkdir .themes
cd .themes
git clone git@github.com:anmoljagetia/Flatabulous.git
```

### Install the Source Code Pro font

[Source Code Pro](https://github.com/adobe-fonts/source-code-pro) is a lovely monospaced font that is given away for free by Adobe.

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

[Powerline fonts](https://github.com/powerline/fonts) are used to create attractive text effects in the emacs powerline and also in your shell. Have you ever seen people who have really sweet looking shells with colorful backgrounds and colored arrows and stuff on the prompt? Powerline fonts are how that happens.

``` shell
mkdir -p ~/src/powerline/
cd ~/src/powerline
git clone https://github.com/powerline/fonts
cd fonts
./install.sh
```

## Tools

### VM Tools

I use a combination of Docker and QEMU to handle virtual environments. These tools make it easy to work with VMs of various sorts.

In no particular order, the tools are:

* [Remmina](https://www.remmina.org/wp/) - A *nix remote desktop client
* [QEMU](http://www.qemu-project.org/) - Machine emulator and virtualization.
* [Docker](https://www.docker.com/) - Run apps, not VMs. Head over to [What is Docker?](https://www.docker.com/what-docker) to learn more about what Docker is and how it can help.
* [AUFS](https://en.wikipedia.org/wiki/Aufs) and `aufs-tools` - Docker uses this as a way of abstracting away the file system and working around creating multiple copies of the same core OS.
* [Virtual Machine Manager](https://virt-manager.org/) - puts a GUI on managing virtual machines using Linux KVM. Why don't I use the command line? Simple: I don't use the commands often enough to justify learning them.

To install VM tooling:

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
                 lua5.3 liblua5.3-dev \
                 global \
                 ncurses-term \
                 gnutls-bin libgnutls-dev

ln -s ~/src/peschkaj/dotfiles/.clang-format ~/.clang-format
```

~~After installing emacs, edit `/usr/share/applications/emacs24-lucid.desktop` to point to the `/usr/local/bin/emc.sh` script.~~

The gitconfig settings above rely on using Helix Merge Tool (formerly P4Merge). Download the right version for your platform from [the Helix Merge Tool download page](https://www.perforce.com/downloads/helix#clients). I use the "Helix P4V: Visual Client".

``` shell
mkdir -p ~/.local/p4merge
tar xzf ~/Downloads/p4v.tgz -C ~/.local/p4merge
```

Make sure that `.local/p4merge/bin` is in your `PATH`. If you're using ZSH and my `.zshenv` file, this won't be a problem.

### Java

But only if you want it...

``` shell
sudo add-apt-repository ppa:webupd8team/java
sudo apt update
sudo apt install oracle-java8-installer
```


### Emacs

First install the emacs dependencies:

``` shell
sudo apt-get -qq install -y stow build-essential libx11-dev xaw3dg-dev \
      libjpeg-dev libpng-dev libgif-dev libtiff5-dev libncurses5-dev \
      libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev \
      libxml2-dev libgpm-dev libotf-dev libm17n-dev \
      libgnutls-dev wget
```

A current version of emacs can be installed through [build-emacs.sh](https://github.com/peschkaj/dotfiles/blob/master/build-emacs.sh). You can invoke it with `build-emacs.sh 25.1` inside the `~/src` directory.

I like emacs to start as soon as I'm logged in. We can set up a user systemd dependency to run emacs as a service:

``` shell
mkdir -p ~/.config/systemd/user/

echo "[Unit]
Description=Emacs: the extensible, self-documenting text editor

[Service]
Type=forking
ExecStart=/usr/local/bin/emacs --daemon
ExecStop=/usr/local/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=always

[Install]
WantedBy=default.target
" | tee ~/.config/systemd/user/emacs.service

systemctl --user enable emacs
```

At this point you'll be tempted to start emacs. Don't. Just wait.

#### Spacemacs

[spacemacs](https://spacemacs.org) is an emacs starter kit on steroids. Just visit their home page if you want to know more. 

**N.B.** First emacs launch is going to take _forever_. If any of the ELPA repositories are down, this launch will fail until they're available.

``` shell
cd ~
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s ~/src/peschkaj/dotfiles/.spacemacs .spacemacs
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/irony-mode ~/.emacs.d/private/irony-mode
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/mineo-rtags ~/.emacs.d/private/mineo-rtags
ln -s ~/src/peschkaj/dotfiles/spacemacs-private/snippets ~/.emacs.d/private/snippets

sudo cp ~/src/peschkaj/dotfiles/emc.sh /usr/local/bin/emc.sh
```

Sometimes the emacs history saving feature goes crazy and generates monster history files. The side effect of this is that emacs will randomly pause for several seconds while it saves huge amounts of data to disk. The solution is to use `logrotate` to automatically rotate out log files. Add the following to `/etc/logrotate.d/emacs.savehist`

``` shell
/home/jeremiah/.emacs.d/.cache/savehist {
    weekly
    rotate 5
    compress
    missingok
    create 0644 jeremiah jeremiah
    su jeremiah jeremiah
}
```

Test with `sudo logrotate -f -v /etc/logrotate.d/emacs.savehist`


#### rtags

```
cd ~/src
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
mkdir build
cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DRTAGS_NO_BUILD_CLANG=1 ..
make
sudo make install
```

~~It may not be necessary to use the `LIBCLANG_LLVM_CONFIG_EXECUTABLE`, depending on how everything is installed by packages.~~

Create a user daemon according to [Integration with `systemd`](https://github.com/Andersbakken/rtags#integration-with-systemd-gnu-linux)

#### Back to emacs

OK, now you can start emacs, but do it from the command line using `/usr/local/bin/emacs`. Wait for it to install all of the spacemacs configuration. Once that's done, exit emacs. If you're using my emacs/spacemacs configuration, this will leave emacs running in the background. Get rid of that with `killall emacs`. 

_Now_ you can start emacs as a service with `systemctl --user enable emacs`

### Rust

Optional, but if you want Rust on your system, this will do it.

This downloads the Rust sources as well, this is only a requirement if you want source code completion through `racer`. If you neither know nor care, don't worry about it. I usually use nightly Rust because reasons, if you want a more stable Rust, don't run the last command.

```
curl https://sh.rustup.rs -sSf | sh
mkdir -p ~/src/rust-lang
cd ~/src/rust-lang
git clone git@github.com:rust-lang/rust.git

rustup default nightly
```

### Haskell

Who doesn't love Haskell?

``` shell
sudo apt install haskell-platform haskell-platform-doc ghc-doc haskell-doc 
cabal update
cabal install apply-refact hlint stylish-haskell hasktags hoogle

ln -s ~/src/peschkaj/dotfiles/ghci ~/.ghci
```

Optionally install Haskell Stack

``` shell
wget -qO- https://get.haskellstack.org/ | sh
```

### Pandoc

[Pandoc](http://pandoc.org/) lets you convert just about any document format into just about any other document format. The only downside is that `texlive-xetex` pulls in about 1.2GB of "stuff", so be prepared to a lengthy install. Like... go make a sandwich. 

``` shell
sudo apt install pandoc texlive-xetex
```

### Global git set up

This sets up a bunch of global git configuration on your machine. I'd love to set this up as includes in the `.gitconfig` file, but for some reason, it won't work and I've never figured out why. Instead, I just use a heredoc and the magic of `cat`.

**NB** This assumes you've also cloned the `seekrets` repository earlier. Or, if you're not future me, that you have a `seekrets` repository.

```
ln -s ~/src/peschkaj/dotfiles/GIT_IGNORE ~/.gitignore_global

cat << EOF > ~/.gitconfig
[include]
    path = /home/jeremiah/src/peschkaj/dotfiles/git/gitconfig
[include]
    path = /home/jeremiah/src/peschkaj/seekrets/git/gitconfig-github
[include]
    path = /home/jeremiah/src/peschkaj/dotfiles/git/gitconfig-linux

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

Substitute "Dropbox" above if you're not an `insync`/Google Drive user.

## Useful Scripts

``` shell
ln -s ~/src/peschkaj/dotfiles/should-i-reboot.sh .local/bin/should-i-reboot
```

## Graphics

### NVidia Graphics Driver PPAs

Check NVidia to see what the most recent driver is and install that.

You can also run `apt search --names-only nvidia` and scan the list. As of 2017-03-20, the current nvidia driver is `nvidia-378`.

### bumblebee configuration (optional)

**N.B** This is only necessary if you have a laptop with Optimus (a discrete graphics card _and_ integrated graphics card). If you don't, skip it.

This should install bumblebee and friends, but if it doesn't:

``` shell
sudo apt install prime-indicator \
                 bbswitch \
                 bbswitch-dkms \
                 primus
```

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

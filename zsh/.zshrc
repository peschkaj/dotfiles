# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"
#ZSH_THEME="kolo"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(osx git emacs history cmake gnu-utils git-open stack)

fpath+=~/.zfunc

# User configuration
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.

# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="emacs ~/.zshrc"
alias ohmyzsh="emacs ~/.oh-my-zsh"

alias ls="ls --color"

export RUST_NEW_ERROR_FORMAT=true

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c -a emacs"
alias findgrep='find . -type f -print0 | xargs -0 grep -I -H -n --color=always'

# SSH aliases
alias pdxada='ssh -i ~/.ssh/id_rsa jpeschka@ada.cs.pdx.edu -t $@ "tmux attach || tmux new"'
alias pdxlinux='ssh -i ~/.ssh/id_rsa jpeschka@linux.cs.pdx.edu -t $@ "tmux attach || tmux new"'
alias pdxlinuxlab='ssh -i ~/.ssh/id_rsa jpeschka@linuxlab.cs.pdx.edu  -t $@ "tmux attach || tmux new"'
alias babbage='ssh -i ~/.ssh/id_rsa jpeschka@babbage.cs.pdx.edu -t $@ "tmux attach || tmux new"'
alias pdx333grader='ssh -i ~/.ssh/id_rsa cs333_1@linuxlab.cs.pdx.edu'

alias vout="valgrind --track-origins=yes --leak-check=full ./a.out"

## Colorize man pages
man() {
    env \
        LESS_TERMCAP_mb=$'\e[1;94m' \
        LESS_TERMCAP_md=$'\e[1;94m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[1;44;33m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[1;32m' \
        man "$@"
}


upgrade_mssql() {
    if [[ uname -eq "Darwin" ]]; then
        docker pull microsoft/mssql-server-linux:2017-latest
        docker stop mssql
        docker rm mssql
        docker run --name=mssql -i -e ACCEPT_EULA=Y -e SA_PASSWORD=Password23 -p 1433:1433 --volumes-from mssql-data -v /Users/jeremiah/Documents:/Documents -d microsoft/mssql-server-linux:2017-latest
    else
        docker pull microsoft/mssql-server-linux:2017-latest
        docker stop mssql
        docker rm mssql
        docker run -v /opt/docker/volumes/mssql:/var/opt/mssql --name=mssql -i -e ACCEPT_EULA=Y -e SA_PASSWORD=Password23 -p 1433:1433 -d microsoft/mssql-server-linux:2017-latest
    fi
}

colors() {
    T='gYw'   # The test text

    echo -e "\n                 40m     41m     42m     43m\
     44m     45m     46m     47m";

    for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
                       '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
                       '  36m' '1;36m' '  37m' '1;37m';
    do FG=${FGs// /}
       echo -en " $FGs \033[$FG  $T  "
       for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
       do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
       done
       echo;
    done
    echo
}

export PATH="/usr/local/opt/llvm/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
export CFLAGS="-march=native"
export CPPFLAGS="-I/usr/local/opt/llvm/include"

export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
alias top=htop

# networking
alias flushcache='dscacheutil -flushcache'
alias ssh='ssh -A'

# git command aliases
# alias gs='git status '
# alias ga='git add '
# alias gb='git branch '
# alias gc='git commit'
# alias gd='git diff'
# alias go='git checkout '
# alias gk='gitk --all&'
# alias gx='gitx --all'
# alias gl='git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short'
# alias gh="open \`git remote -v | grep git@github.com | grep fetch | head -1 | cut -f2 | cut -d' ' -f1 | sed -e's/:/\//' -e 's/git@/http:\/\//'\`"

# git typo fixes
alias got='git '
alias get='git '

# Quick way to rebuild the Launch Services database and get rid
# of duplicates in the Open With submenu.
alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

# emacs
### EMACS configuration brought to you by: https://gist.github.com/1120833
alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
alias emacs="emacsclient -n "
alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -c -a emacs"    # start a windowed frame
alias em="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a emacs -nw"   # start a terminal frame
alias ea="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -a emacs"       # do not start a new frame
alias et="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t"

export EDITOR='subl -w'
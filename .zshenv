export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$HOME/.local/bin:$HOME/.local/p4merge/bin:$HOME/.cabal/bin:$PATH:$HOME/bin
export CARGO_HOME=$HOME/.cargo
export PATH=$PATH:$CARGO_HOME/bin

export RUSTFLAGS="-C target-cpu=native"

# Temporary hack for https://github.com/syl20bnr/spacemacs/issues/10906
#source ~/.zshrc

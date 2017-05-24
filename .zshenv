export PATH="/home/jeremiah/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/home/jeremiah/bin"
export PATH=$HOME/.local/bin:$HOME/.local/p4merge/bin:$HOME/.cabal/bin:$PATH
export CARGO_HOME=/home/jeremiah/.cargo
export RUST_SRC_PATH=~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
#export RUST_SRC_PATH=/home/jeremiah/src/rust-lang/rust/src

export SCALA_HOME=/usr/local/share/scala
export PATH=$PATH:$SCALA_HOME/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f /home/jeremiah/Downloads/google-cloud-sdk/path.zsh.inc ]; then
  source '/home/jeremiah/Downloads/google-cloud-sdk/path.zsh.inc'
fi

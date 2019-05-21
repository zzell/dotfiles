#### emacs
```sh
# latest release
sudo add-apt-repository -y ppa:ubuntu-elisp \
  && sudo apt-get update \
  && sudo apt-get install emacs-snapshot
```

#### vim
```sh
# rm vim-tiny and install latest release
sudo apt -y remove vim-tiny \
  && sudo add-apt-repository ppa:jonathonf/vim -y \
  && sudo apt update \
  && sudo apt install vim

# get and initialize vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim \
  && vim +PluginInstall +qall
```

#### ack
```sh
sudo apt-get install ack-grep \
  && sudo dpkg-divert --local --divert /usr/bin/ack --rename --add /usr/bin/ack-grep
```

#### git
*  use `ssh` instead of `https://`
```sh
git config --global url."git@github.com:".insteadOf "https://github.com/"
```

#### tmux
* shared session via ssh
```sh
# start session
tmux -S /tmp/tmux-socket new-session
# attach
ssh -t user@host tmux -S /tmp/tmux-socket attach
```

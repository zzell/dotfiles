#### docker
```sh
# works for ubuntu 18.04
curl -fsSL get.docker.com | sh \
  && sudo usermod -a -G docker $USER
  # reboot
```

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
```

#### git
*  use `ssh` instead of `https://`
```sh
git config --global url."git@github.com:".insteadOf "https://github.com/"
# generate key
ssh-keygen -t rsa -b 4096 -C "sukhodolskyi.97@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
```

#### staff
```sh
# | xclip -selection clipboard
sudo apt install xclip

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

#### generate keys pair
https://help.github.com/en/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent

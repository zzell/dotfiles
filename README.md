
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



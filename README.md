# dotfiles

tmux shared session via ssh
```sh
# serverside
tmux -S /tmp/shared-tmux-socket new-session

# client 
ssh -t machine0 tmux -S /tmp/shared-tmux-socket attach
```

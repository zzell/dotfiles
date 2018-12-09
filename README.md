
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

#### jetbrains license
* is it legal?
```sh
# get container with license server and config it to run on every system startup
docker run -d --name jetbrains --restart=always ansonliao/jetbrains-license-server:1.2
# get container's ip address - license server
docker inspect -f "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" jetbrains
# get container's port
docker ps
# license server: http://addr:port
```

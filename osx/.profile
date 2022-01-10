# HISTORY
# PROMPT_COMMAND='history -a'
# HISTTIMEFORMAT='%F %T '
# HISTCONTROL=ignoredups
# HISTIGNORE=?:??
# shopt -s cmdhist # attempt to save all lines of a multiple-line command in the same history entry
# shopt -s lithist # save multi-line commands to the history with embedded newlines

HISTFILESIZE=-1
shopt -s histappend # append to history, don't overwrite it
export PROMPT_COMMAND='history -a'

# go
export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin
# export GOROOT=/usr/local/go

export GOROOT="$HOME/homebrew/opt/go@1.15/libexec"
export GOSUMDB=off

export EDITOR=vim
export CLICOLOR=1
export LC_ALL=en_US.UTF-8

# custom binaries
export PATH=$PATH:$HOME/bin

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# aliases
alias grep='grep --color=auto'
# git 
alias gb="git branch --show-current"
alias g="git"
alias mastermerge="git checkout master && git pull && git checkout - && git merge master"
alias gcm="git commit -m"
alias gch="git checkout"
alias gs="git status"
alias ga="git add -A"
alias gp="git push origin HEAD"
alias gpl="git pull"
alias gl="git log --oneline"
alias gd="git diff"
alias gr="git reset --hard && git clean -fd"

# other
alias ll="ls -la"
alias l="ls -la"
alias k="kubectl"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export PATH="/usr/local/opt/libxml2/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/libxml2/lib"
export CPPFLAGS="-I/usr/local/opt/libxml2/include"
export BASH_SILENCE_DEPRECATION_WARNING=1

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# complete -C /usr/local/bin/terraform terraform


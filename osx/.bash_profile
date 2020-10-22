# go
export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin

export EDITOR=vim
export CLICOLOR=1
#export LSCOLORS=GxBxCxDxexegedabagaced
export LC_ALL=en_US.UTF-8

#
export PATH=$PATH:$HOME/bin

export PATH=$PATH:$HOME/flutter/bin

parse_git_dirty () {
	[[ $(git status 2> /dev/null | tail -n1 | cut -c 1-17) != "nothing to commit" ]] && echo "*"
}

parse_git_branch () {
	git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ \1$(parse_git_dirty)/"
}

__prompt_command() {
	PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w'
	PS1+="\[\033[01;35m\]\$(parse_git_branch)"
	PS1+="\[\033[00m\]\$ "
}

PROMPT_COMMAND=__prompt_command

# aliases
alias g="git"
alias mastermerge="git checkout master && git pull && git checkout - && git merge master"
alias gcm="git commit -m"
alias gch="git checkout"
alias gs="git status"
alias ga="git add -A"
alias gp="git push origin HEAD"
alias gl="git log --oneline"
alias gd="git diff"
alias ll="ls -la"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

export PATH="/usr/local/opt/libxml2/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/libxml2/lib"
export CPPFLAGS="-I/usr/local/opt/libxml2/include"

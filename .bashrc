export EDITOR=vim

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
alias mastermerge="git checkout master && git pull && git checkout - && git merge master"

# go
export GOPATH=$HOME/go

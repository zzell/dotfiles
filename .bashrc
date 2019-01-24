export EDITOR=vim
export TERM=screen-256color

parse_git_dirty () {
	[[ $(git status 2> /dev/null | tail -n1 | cut -c 1-17) != "nothing to commit" ]] && echo "*"
}

parse_git_branch () {
	git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/*\(.*\)/\1$(parse_git_dirty)/"
}

export NONE='\[\e[0m\]'
export color_red='\[\033[1;38;5;9m\]'
export color_white='\[\033[1;38;5;231m\]'
export color_purple='\[\033[1;38;5;141m\]'
export color_yellow='\[\033[1;38;5;220m\]'

__prompt_command()
{
	local EXIT="$?"

	PS1="${color_red}\u"
	PS1+=" ${color_yellow}\w"
	PS1+="${color_purple}\$(parse_git_branch)"

	if [ $EXIT != 0 ]; then
		PS1+="${color_red}> ${NONE}"
	else
		PS1+="${color_white}> ${NONE}"
	fi
}

PROMPT_COMMAND=__prompt_command

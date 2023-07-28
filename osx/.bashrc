# Suppressing "The default interactive shell is now zsh" message in macOS Catalina
export BASH_SILENCE_DEPRECATION_WARNING=1

# make bash append rather than overwrite the history on disk
shopt -s histappend

# the number of lines tat are stored in history file
export HISTFILESIZE=999999

# the number of entries that are stored in memory
export HISTSIZE=99999

# history -a causes the last command to be written to the history file automatically 
export PROMPT_COMMAND='history -a'

export EDITOR=vim
export CLICOLOR=1
export LC_ALL=en_US.UTF-8
export PATH=$HOME/bin:$PATH # custom binaries

# Set colors for less. Borrowed from https://wiki.archlinux.org/index.php/Color_output_in_console#less
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

# ----------------------------- GOLANG -----------------------------

export GO111MODULE="auto"
export GOSUMDB=off
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/go/go1.19.7/bin
# export PATH=$PATH:$HOME/go/bin

# ----------------------------- PROMPT -----------------------------

# \[ \033 [<format> ; <color_code>m \]
parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/'
}

PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;91m\]\$(parse_git_branch)\[\033[00m\]$ "

# ----------------------------- ALIASES -----------------------------

alias k="kubectl"
alias l="ls -la"
alias ls="ls -GF"
alias ll="ls -GFalh"
alias cwd='printf "%q\n" "$(pwd)" '
alias less='less -R' # with colors (raw control chars)
alias grep='grep --color=auto'

h() {
  grep "$*" ~/.bash_history --color=always | less -R +G
}

# ----------------------------- GIT -----------------------------

alias gb="git branch --show-current"
alias g="git"
alias gf="git fetch"
alias mastermerge="git checkout master && git pull && git checkout - && git merge master"
alias gcm="git commit -m"
alias gch="git checkout"
alias gs="git status"
alias ga="git add -A"
alias gp="git push origin HEAD"
alias gl1="git log --oneline -1"
alias gd="git diff"
alias gr="git reset --hard && git clean -fd"
gpl() { git pull origin "$(git branch --show-current)"; }

# ----------------------------- DOCKER -----------------------------

alias dps="docker ps --format \"table {{.ID}}\t{{.Names}}\t{{.Image}}\""

# ----------------------------- OTHER -----------------------------

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export PATH="/usr/local/opt/libxml2/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/libxml2/lib"
export CPPFLAGS="-I/usr/local/opt/libxml2/include"
export BASH_SILENCE_DEPRECATION_WARNING=1

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
eval "$(/opt/homebrew/bin/brew shellenv)"


# #setup terminal tab title
# function title {
#     if [ "$1" ]
#     then
#         unset PROMPT_COMMAND
#         echo -ne "\033]0;${*}\007"
#     else
#         # export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'
#         export PROMPT_COMMAND='echo -ne "\033]0;${XX/#$HOME/~}\007"'
#     fi
# }


# #setup terminal tab title
# function title {
#     if [ "$1" ]
#     then
#         unset PROMPT_COMMAND
#         echo -ne "\033]0;${*}\007"
#     else
#         # export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'
#         export PROMPT_COMMAND='echo -ne "\033]0;${XX/#$HOME/~}\007"'
#     fi
# }

# title
# 
# PROMPT_COMMAND='echo -n "$(date) > "'
. "$HOME/.cargo/env"

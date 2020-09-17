#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias vi=vim
alias ls='ls --color=auto'
alias ll="ls -lv --group-directories-first"
alias la='ll -A'

# for managing dotfiles
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# prompt
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\u@\h \[\e[32m\]\w \[\e[91m\]\$(parse_git_branch)\[\e[00m\]$ "

# path
export PATH=$PATH:$HOME/bin

# for maintaining iex history across sessions
export ERL_AFLAGS="-kernel shell_history enabled"


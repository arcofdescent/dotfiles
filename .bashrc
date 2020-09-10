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
PS1='[\u@\h \W]\$ '

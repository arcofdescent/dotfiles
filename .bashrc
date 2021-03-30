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
alias rm="rm -i"

# for managing dotfiles
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# prompt
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\u@\h \[\e[32m\]\w \[\e[91m\]\$(parse_git_branch)\[\e[00m\]$ "

# path
export PATH=$PATH:$HOME/bin:/usr/local/go/bin
export NPM_TOKEN=5140183b8e6f6c478f323ae7daef25e9d1f7e303

# for maintaining iex history across sessions
export ERL_AFLAGS="-kernel shell_history enabled"

export HISTSIZE=3000
export HISTFILESIZE=3500
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

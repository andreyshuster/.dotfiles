# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="bira"
ZSH_THEME="robbyrussell"
#ZSH_THEME='mrtazz'

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git) # rvm rails ruby gem bundler)

source $ZSH/oh-my-zsh.sh

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:/usr/local/share/npm/bin:$HOME/utils:

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
# [[ $- != *i* ]] && return
# [[ $TERM != screen* ]] && exec tmux -u

# Customize to your needs...
alias tmux="tmux -u"
alias vim="/usr/local/bin/vim"
alias emacs="/usr/local/bin/emacs"
alias emacsnw="emacs -nw"
export TERM=screen-256color

export LC_ALL=en_US.UTF-8

export VISUAL='emacs -nw'
export EDITOR="$VISUAL"

[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM

export NODE_PATH='/usr/local/lib/node_modules'

#export WORKON_HOME=$HOME/.virtualenvs
#export PROJECT_HOME=$HOME/Projects
#export GOPATH=$HOME/Projects/Go
#source /usr/local/bin/virtualenvwrapper.sh

export PATH=/Users/andreyshuster/small-tools:/usr/local/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
#source '/Users/novus42/google-cloud-sdk/path.zsh.inc'
#source '/Users/novus42/google-cloud-sdk/completion.zsh.inc'
#eval "$(rbenv init -)"

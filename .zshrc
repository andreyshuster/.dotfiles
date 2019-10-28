# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

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
plugins=(git zsh-autosuggestions) # rvm rails ruby gem bundler)

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:/usr/local/share/npm/bin:$HOME/utils:$HOME/small-tools:

source $ZSH/oh-my-zsh.sh
#source /usr/local/bin/aws_zsh_completer.sh

# Customize to your needs...
alias tmux="tmux -u"
alias e="emacs -nw"
alias ec="emacsclient -nw"
alias readlink="greadlink"

export TERM="xterm-256color"

export VISUAL='emacs -nw'
export EDITOR="$VISUAL"

# virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
# source ~/.local/bin/virtualenvwrapper.sh
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
source /usr/local/bin/virtualenvwrapper.sh

export NODE_PATH='/usr/local/lib/node_modules'

export PATH="$HOME/.yarn/bin:$PATH"

# settings that not supposed to be on every machine e
LOCAL_ZSHRC="$HOME/.zshrc-local"
[ -f $LOCAL_ZSHRC ] && source $LOCAL_ZSHRC

export GEM_HOME=$HOME/gems
export PATH=$HOME/gems/bin:$PATH


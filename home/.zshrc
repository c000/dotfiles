# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory extendedglob nomatch notify
unsetopt autocd beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -Uz zargs

# Histories
setopt correct
setopt hist_reduce_blanks
setopt hist_ignore_all_dups
setopt extended_history
setopt share_history
setopt hist_ignore_space

# CDR
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 1000

zstyle ':completion:*:default' menu select
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}'
zstyle ':completion:*' format '%K{yellow}%F{black}%d%f%k'

if { which "nvim" > /dev/null }; then
  export EDITOR=nvim
elif { which "vim" > /dev/null }; then
  export EDITOR=vim
fi

setopt prompt_subst
PROMPT='%K{green}%F{black}%n %m:%~%K{blue}%h%1(j.%%%j.)%0(?..:%?)%K{red}${DOCKER_HOST}%f%k%# '

alias ls='ls --color=auto'
alias -g L='| less'
export LESS='-R -W -X'

# Auto ls
chpwd() {
  ls
}

# Golang
#export GOROOT=~/.go/go1.11.6
#PATH=$PATH:$GOROOT/bin
if { which "go" > /dev/null }; then
  export GOPATH=~/go
  export PATH=$PATH:$GOPATH/bin
fi

# Haskell
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.cabal/bin

# Node
if { which "node" > /dev/null }; then
  export PATH=$PATH:$HOME/.node_modules/bin
fi

# Rust
if [[ -f "$HOME/.cargo/env" ]]; then
  . "$HOME/.cargo/env"
fi
if [[ -d "$HOME/.cargo/bin" ]]; then
  PATH="$PATH:$HOME/.cargo/bin"
fi

# Deno
if [[ -d "$HOME/.deno/bin" ]]; then
  PATH="$PATH:$HOME/.deno/bin"
fi

# FZF
if [[ -f ~/.fzf.zsh ]]; then
  . ~/.fzf.zsh
fi
if { which "fzf" > /dev/null }; then
  export FZF_DEFAULT_OPTS='--height 40% --border --reverse'
  __fzf::cdr() {
    local s="$(cdr -l | perl -pe 's/^[0-9]*\s*//' | fzf +s -e)"
    [[ -n $s ]] && print "cd ${(q)s}"
  }
  __fzf::cdr::zle() {
    local s="$(__fzf::cdr)"
    if [[ -n $s ]]
    then
      BUFFER="$s"
      zle reset-prompt
      zle accept-line
    else
      zle reset-prompt
    fi
  }
  zle -N __fzf::cdr::zle
  bindkey '^s' __fzf::cdr::zle
  stty stop undef

  __fzf::history() {
    local s="$(history -n 0 | fzf --tac +s -e -q "${BUFFER}")"
    [[ -n $s ]] && BUFFER="$s"
    CURSOR=$#BUFFER
    zle reset-prompt
  }
  zle -N __fzf::history
  bindkey '^r' __fzf::history
fi

# Direnv
if { which "direnv" > /dev/null }; then
  eval "$(direnv hook zsh)"
fi

if [[ -d "$HOME/.anyenv/bin" ]]; then
  PATH="$HOME/.anyenv/bin:$PATH"
  eval "$(anyenv init -)"
fi

if [[ -f "$HOME/.poetry/env" ]]; then
  source "$HOME/.poetry/env"
fi

# Azure cli
PATH=$PATH:$HOME/bin

if [[ -f "$HOME/.homesick/repos/homeshick/homeshick.sh" ]]; then
  source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fi

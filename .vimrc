set nocompatible
filetype on
filetype off

" Setting up Vundle if not installed
let iCanHazVundle=1
let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
    echo 'Installing Vundle..'
    echo ''
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
    let iCanHazVundle=0
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'kien/ctrlp.vim'
Bundle 'klen/python-mode'
Bundle 'tomasr/molokai'
Bundle 'morhetz/gruvbox'
Bundle 'tpope/vim-fugitive'
Bundle 'mattn/emmet-vim'
Bundle 'tpope/vim-surround'
Bundle 'jszakmeister/vim-togglecursor'
Bundle 'majutsushi/tagbar'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'honza/vim-snippets'
Bundle 'altercation/vim-colors-solarized'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Bundle 'mhinz/vim-startify'
Plugin 'mileszs/ack.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
"Bundle 'Valloric/YouCompleteMe'

silent !type jshint &>/dev/null || { cd ~; echo 'Installing jshint'; npm install -g jshint; cd -; }

filetype plugin indent on

source ~/.vim/vimrc
colors zenburn
syntax enable

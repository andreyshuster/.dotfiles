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
"""
Bundle 'kien/ctrlp.vim'
Bundle 'tomasr/molokai'
Bundle 'tpope/vim-fugitive'
Bundle 'mattn/emmet-vim'
Bundle 'tpope/vim-surround'
Bundle 'jszakmeister/vim-togglecursor'
Bundle 'majutsushi/tagbar'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle 'altercation/vim-colors-solarized'
"Bundle 'fatih/vim-go'
"Bundle 'Valloric/YouCompleteMe'
silent !type jshint &>/dev/null || { cd ~; echo 'Installing jshint'; npm install -g jshint; cd -; }

filetype plugin indent on

source ~/.vim/vimrc
colorscheme molokai
let g:molokai_original=1
syntax enable

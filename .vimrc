" Global options
set nocompatible

" Command line options
set ch=2
set showcmd
set history=50

" Text editing options
set encoding=utf-8
set nu
set noautoindent
set bs=1
set ru
set tabstop=4
set shiftwidth=4
"set expandtab
set backspace=indent,eol,start
set ruler
set scrolloff=10
set clipboard=unnamedplus

" Set search options
set hlsearch
set incsearch

" Colour scheme options
syntax enable
set background=dark
colorscheme delek

" Coding options
syn on

" Backup to ~/.tmp 
set backup 
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp 
set backupskip=/tmp/*,/private/tmp/* 
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp 
set writebackup


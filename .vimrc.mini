"                                                          _
"                                                   __   _(_)_ __ ___  _ __ ___
"                                                   \ \ / / | `_ ` _ \| '__/ __|
"                                                    \ V /| | | | | | | | | (__
"                                                     \_/ |_|_| |_| |_|_|  \___|
" ------------------------------------------------------------------------------
set nocompatible                                      "turn off Vi compatibility
set backup
set swapfile
set undofile
silent call mkdir ($HOME."/.vim.backup", "p")
set backupdir=~/.vim.backup//,.
set directory=~/.vim.backup//,.
set undodir=~/.vim.backup//,.
" moving around, searching and patterns ----------------------------------------
set incsearch                                  "show search pattern while typing
set showmatch                                                "highlight brackets
set smartcase                                                "if caps match case
set ignorecase                                         "if lowercase ignore case
set autochdir                             "change to directory of file in buffer
" displaying text --------------------------------------------------------------
set lazyredraw
set number
set wrap
set linebreak                                                    "wrap btw words
set scrolloff=6
" syntax, highlighting and spelling --------------------------------------------
filetype plugin indent on
syntax on
set hlsearch                                     "highlight prev search patterns
set background=dark                                                      
" multiple windows -------------------------------------------------------------
set hidden                                                   "bg unsaved buffers
" multiple tab pages -----------------------------------------------------------
" terminal ---------------------------------------------------------------------
set ttyfast                                            "fast terminal connection
" using the mouse --------------------------------------------------------------
set mousehide
set mouse=a
" messages and info ------------------------------------------------------------
set noerrorbells visualbell t_vb=
set showmode                                               "display current mode
set showcmd                                          "show commands while typing
set ruler                                                  "show cursor position
" editing text -----------------------------------------------------------------
set wrapmargin=8                 "margin from the right in which to break a line
set formatoptions=tcqrn1
set textwidth=79                        "line length above which to break a line
set nrformats-=octal                       "0-prefixed numbers are still decimal
set backspace=indent,eol,start                               "proper backspacing
" tabs and indenting -----------------------------------------------------------
set softtabstop=2
set shiftwidth=2
set tabstop=2
set autoindent
set smartindent
set smarttab
set expandtab
set shiftround                              "round >> to multiples of shiftwidth
" folding ----------------------------------------------------------------------
set foldmethod=marker
set foldmarker={{{,}}}
" mapping ----------------------------------------------------------------------
set timeout                                     "fix slow 'O' insert (all three)
set timeoutlen=1000
set ttimeoutlen=100
" command line editing ---------------------------------------------------------
set wildmenu
set wildmode=full
" running make and jumping to errors -------------------------------------------
set makeprg=make                           "program used for the ":make" command
" multi-byte characters --------------------------------------------------------
set encoding=utf-8
set fileencodings=utf-8
" various ----------------------------------------------------------------------
set gdefault                          "use the g flag for :substitute by default
" functions --------------------------------------------------------------------
function! PositionCursorFromViminfo()              "restore last cursor position
  if !(bufname("%") =~ '\(COMMIT_EDITMSG\)')
        \ && line("'\"") > 1
        \ && line("'\"") <= line("$")
    exe "normal! g`\""
  endif
endfunction
" autocommands -----------------------------------------------------------------
autocmd BufReadPost * call PositionCursorFromViminfo()
autocmd GUIEnter * set visualbell t_vb=                     "rm bells and splash
augroup vimscript                                              "vimrc autosource
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

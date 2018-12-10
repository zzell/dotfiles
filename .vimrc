"                                                          _
"                                                   __   _(_)_ __ ___  _ __ ___
"                                                   \ \ / / | `_ ` _ \| '__/ __|
"                                                    \ V /| | | | | | | | | (__
"                                                     \_/ |_|_| |_| |_|_|  \___|

" important --------------------------------------------------------------------
set nocompatible                                      "turn off Vi compatibility
set pastetoggle=<F2>
" vundle -----------------------------------------------------------------------
filetype off                                                           "required
set rtp+=~/.vim/bundle/Vundle.vim                        "where to store plugins
call vundle#begin()                                                "vundle begin
Plugin 'VundleVim/Vundle.vim'                                   "must come first

"Plugin 'w0rp/ale'                                                      "linting
"Plugin 'valloric/youcompleteme'                                   "autocomplete
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }         "FZF
Plugin 'junegunn/fzf.vim'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdcommenter'                                      "comments
Plugin 'scrooloose/nerdtree'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'                                            "snippets
Plugin 'Raimondi/delimitMate'                                "autocolse brackets
Plugin 'itchyny/vim-cursorword'                "underlines the word under cursor
Plugin 'godlygeek/tabular'                                        "align columns
Plugin 'tpope/vim-fugitive'                                                 "git
Plugin 'junegunn/gv.vim'                                                    "git
Plugin 'mattn/emmet-vim'
Plugin 'tpope/vim-surround'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-bufferline'
                                                                   "lang support
Plugin 'fatih/vim-go'
Plugin 'pangloss/vim-javascript'
Plugin 'ternjs/tern_for_vim'             "run npm install in bundle/tern_for_vim
Plugin 'jelera/vim-javascript-syntax'
Plugin 'mxw/vim-jsx'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/html5.vim'
Plugin 'StanAngeloff/php.vim'
                                                                         "colors
Plugin 'KabbAmine/yowish.vim'
Plugin 'lxmzhv/vim'
Plugin 'ajh17/Spacegray.vim'
Plugin 'nanotech/jellybeans.vim'
Plugin 'djjcast/mirodark'
call vundle#end()

" moving around, searching and patterns ----------------------------------------
set incsearch                                  "show search pattern while typing
set showmatch                                                "highlight brackets
set smartcase                                                "if caps match case
set ignorecase                                         "if lowercase ignore case
set autochdir                             "change to directory of file in buffer
" tags -------------------------------------------------------------------------
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
set cursorline                          "highlight the screen line of the cursor
" multiple windows -------------------------------------------------------------
set hidden                                                   "bg unsaved buffers
" multiple tab pages -----------------------------------------------------------
" terminal ---------------------------------------------------------------------
set ttyfast                                            "fast terminal connection
" using the mouse --------------------------------------------------------------
set mousehide
set mouse=a
" printing ---------------------------------------------------------------------
" messages and info ------------------------------------------------------------
set noerrorbells visualbell t_vb=
set showmode                                               "display current mode
set showcmd                                          "show commands while typing
set ruler                                                  "show cursor position
" selecting text ---------------------------------------------------------------
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
" diff mode --------------------------------------------------------------------
" mapping ----------------------------------------------------------------------
set timeout                                     "fix slow 'O' insert (all three)
set timeoutlen=1000
set ttimeoutlen=100
" reading and writing files ----------------------------------------------------
set backup
" the swap file ----------------------------------------------------------------
set swapfile
" command line editing ---------------------------------------------------------
set wildmenu
set wildmode=full
set undofile
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules 
" executing external commands --------------------------------------------------
" running make and jumping to errors -------------------------------------------
set makeprg=make                           "program used for the ":make" command
" language specific ------------------------------------------------------------
" multi-byte characters --------------------------------------------------------
set encoding=utf-8
set fileencodings=utf-8
" various ----------------------------------------------------------------------
set gdefault                          "use the g flag for :substitute by default
set viminfo='100,<9999,s100
" personalization --------------------------------------------------------------
colorscheme elda
set guifont=Lucida_Console:h12:cRUSSIAN::
" functions --------------------------------------------------------------------
function! PositionCursorFromViminfo()              "restore last cursor position
  if !(bufname("%") =~ '\(COMMIT_EDITMSG\)')
        \ && line("'\"") > 1
        \ && line("'\"") <= line("$")
    exe "normal! g`\""
  endif
endfunction
function! RemoveTrailingSpaces()                      "strip whitespace from eof
  normal! mzHmy
  execute '%s:\s\+$::ge'
  normal! 'yzt`z
endfunction
" autocommands -----------------------------------------------------------------
autocmd BufReadPost * call PositionCursorFromViminfo()
autocmd GUIEnter * set visualbell t_vb=                     "rm bells and splash
augroup vimscript                                              "vimrc autosource
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
augroup _fzf
  autocmd!
  autocmd VimEnter,ColorScheme * call <sid>update_fzf_colors()
augroup END
" plugins ----------------------------------------------------------------------
let g:javascript_plugin_jsdoc = 1                       "pangloss/vim-javascript
let g:javascript_plugin_ngdoc = 1
let g:javascript_plugin_flow = 1
let g:jsx_ext_required = 0                                          "mxw/vim-jsx
let g:spacegray_use_italics = 1                                       "spacegray
let g:NERDSpaceDelims = 1                                         "NERDCommenter
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDTrimTrailingWhitespace = 1
let g:ackprg = 'ag --vimgrep --smart-case'                                   "ag
let g:signify_vcs_list = [ 'git' ]                                  "signify git
let delimitMate_expand_cr=1                                         "delimitmate

function! s:update_fzf_colors()                                  "FZF colors fix
  let rules =
  \ { 'fg':      [['Normal',       'fg']],
    \ 'bg':      [['Normal',       'bg']],
    \ 'hl':      [['String',       'fg']],
    \ 'fg+':     [['CursorColumn', 'fg'], ['Normal', 'fg']],
    \ 'bg+':     [['CursorColumn', 'bg']],
    \ 'hl+':     [['String',       'fg']],
    \ 'info':    [['PreProc',      'fg']],
    \ 'prompt':  [['Conditional',  'fg']],
    \ 'pointer': [['Exception',    'fg']],
    \ 'marker':  [['Keyword',      'fg']],
    \ 'spinner': [['Label',        'fg']],
    \ 'header':  [['Comment',      'fg']] }
  let cols = []
  for [name, pairs] in items(rules)
    for pair in pairs
      let code = synIDattr(synIDtrans(hlID(pair[0])), pair[1])
      if !empty(name) && code != ''
        call add(cols, name.':'.code)
        break
      endif
    endfor
  endfor
  let s:orig_fzf_default_opts =
        \ get(s:, 'orig_fzf_default_opts', $FZF_DEFAULT_OPTS)
  let $FZF_DEFAULT_OPTS = s:orig_fzf_default_opts .
        \ (empty(cols) ? '' : (' --color='.join(cols, ',')))
endfunction

let g:UltiSnipsSnippetsDir = '~/.vim/ultisnips'                       "UltiSnips
let g:UltiSnipsSnippetDirectories = ['ultisnips']
let g:UltiSnipsMappingsToIgnore = ['autocomplete']
let g:UltiSnipsEditSplit = "vertical"

"let g:ycm_complete_in_comments = 1                                "YouCompleteMe
"let g:ycm_collect_identifiers_from_comments_and_strings = 1
"let g:ycm_seed_identifiers_with_syntax = 1
"set completeopt-=preview

"let g:ale_sign_warning = '▲'                                                "ALE
"let g:ale_sign_error = '✗'
"highlight link ALEWarningSign String
"highlight link ALEErrorSign Title

" mapping ----------------------------------------------------------------------
let mapleader=","                                                        "leader
nnoremap <C-h> :tabp<CR>
nnoremap <C-l> :tabn<CR>
nnoremap <leader>: q:i
nnoremap <C-o> :NERDTreeToggle<CR>
nnoremap <leader>t :TagbarToggle<CR>
nnoremap <leader>vi :e $VIM/vimrc<CR>
nnoremap <leader><space> :noh<CR>
nnoremap <leader>s :shell<CR>

nnoremap <leader>a :Ack!
nnoremap <leader>A :Ack! <C-r><C-w><CR>

nnoremap <leader>n :bnext<CR>
nnoremap <leader>p :bprev<CR>
nnoremap <leader><leader> <C-^>

nnoremap ;h :History:<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>f :Files<CR>

let g:UltiSnipsExpandTrigger           = '<Tab>'    "YouCompleteMe and UltiSnips
let g:UltiSnipsJumpForwardTrigger      = '<Tab>'
let g:UltiSnipsJumpBackwardTrigger     = '<S-Tab>'
"let g:ycm_key_list_select_completion   = ['<C-j>']
"let g:ycm_key_list_previous_completion = ['<C-k>']
"let g:ycm_key_list_accept_completion   = ['<C-y>']
"let g:ycm_key_list_accept_completion   = ['<C-y>']

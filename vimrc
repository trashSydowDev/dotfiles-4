"""""""""""""""""""""""""""""""""""""""""""""""""""
""""" This file is mantained by tacla.yamada """"""
""""""""""""""""""""""""""""""""""""""""""""""""""" 


"--------------------------------------------------
" Vundle
"--------------------------------------------------
filetype off 
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle manages vundle
Bundle 'gmarik/vundle'
" My Bundles
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/neosnippet'
Bundle 'Lokaltog/vim-powerline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/syntastic'
Bundle 'altercation/vim-colors-solarized'
"Bundle 'ehamberg/vim-cute-python'
Bundle 'godlygeek/tabular'
Bundle 'paradigm/vim-multicursor'
Bundle 'Raimondi/delimitMate'
Bundle 'sjl/gundo.vim'
Bundle 'vim-scripts/sudo.vim'
Bundle 'vim-scripts/SearchComplete'
Bundle 'dandorman/vim-colors.git'

filetype plugin indent on 

"--------------------------------------------------
" Standard config
"--------------------------------------------------
" Look good on linux:
if has("unix")
  let s:uname = system("echo -n \"$(uname)\"")
  if !v:shell_error && s:uname == "Linux"
    set t_Co=256
    if $TERM =~ '256color'
      " Disable Background Color Erase (BCE) so that 
      " color schemes work properly when Vim is used 
      " inside tmux and GNU screen.  See also
      " http://snk.tuxfamily.org/log/vim-256color-bce.html
      set t_ut=
    endif
  endif
endif

set autoindent smartindent
set ttyfast    " indicates we have a strong 
               " terminal connection
syntax enable
colorscheme solarized
set background=dark
set ic
set relativenumber
" Make vim use absolute numbers in insert mode
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber
set cul
set textwidth=79
" Set tabs as 4 spaces - type :retab for setting a
" file's tabs to spaces
set expandtab        
set shiftwidth=4
set tabstop=4
" Better overall tab key behaviour
set smarttab
" General
set complete=.,w,b,u,U,t,i,d
set completeopt-=preview
set noerrorbells
set wildmenu   " better shell command managing 
set pastetoggle=<F2>
set encoding=utf-8
set hlsearch   " Highlight searches
set incsearch  " Highlight searches as they're typed
set autochdir  " Change working directory to
               " whichever file is open or selected
set noesckeys
" I personally prefer this. But when it's
" convenient you can always change it with 
" :set wrap
set nowrap
set nocompatible
filetype plugin on

"--------------------------------------------------
" GUI
"--------------------------------------------------
set guifont=Consolas:h13


"--------------------------------------------------
" Mappings
"--------------------------------------------------
let mapleader = ","
" Fix some command typos:
cabbrev Q quit
cabbrev W write
cabbrev WQ wq
cabbrev Wq wq
cabbrev wQ wq
" Easily move lines around:
nnoremap - dd\|p
nnoremap _ dd\|k\|P
" Convert current word to uppercase:
inoremap <c-u> <esc>lviwU<esc>i
nnoremap <c-u> <esc>viwU<esc>
" Quickly edit .vimrc:
nnoremap <leader>ev :vs $MYVIMRC<cr>
" Repeat last substitution
nnoremap <leader>r :s<cr>
" File type useful coding stuff:
augroup fileTypeMods
  autocmd!
  autocmd FileType python nnoremap <buffer> <leader>c I# <esc>j0
  autocmd FileType python nnoremap <buffer> <leader>mk :call InterpretPython()<CR>
  autocmd FileType python nnoremap <buffer> <leader>nk :w<CR>:!python % 
  autocmd FileType c nnoremap <buffer> <leader>mk :call CompileRunGcc()<CR>
  " English Spell Checking
  autocmd FileType text set spell
  autocmd FileType text set spelllang=en
augroup END
" Compile or interpret
func! CompileRunGcc()
  exec "w"
  exec "!gcc --std=c99 % -o %<; ./%<"
endfunc
func! InterpretPython()
  exec "w"
  exec "!python %"
endfunc
" Tabs
nnoremap tt :tabnew<CR>
nnoremap tn :tabnext<CR>
nnoremap te :tabedit 
nnoremap tc :tabclose<CR>
nnoremap tn :tabnext<CR>
nnoremap tp :tabprevious<CR>
" Gundo
nnoremap <leader>u :GundoToggle<CR>
" Solarized
let g:solarized_termcolors = 256
" Tabularize
vnoremap <leader>w :Tabularize/
" Clear search
nnoremap <silent><Leader>/ :nohlsearch<CR>
" General tweaks:
nnoremap <leader>g G
vnoremap <leadeR>g G
onoremap p i(
" Make H and L go to the beggining and end of a
" line, respectively.
noremap H ^
noremap L $


"--------------------------------------------------
" Backup (without bloat)
"--------------------------------------------------
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp


"--------------------------------------------------
" Persistent Undo
"--------------------------------------------------
if exists("+undofile")
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
  " :help undo-persistence
  " This is only present in 7.3+
  if isdirectory($HOME . '/.vim/undo') == 0
    :silent !mkdir -p ~/.vim/undo > /dev/null 2>&1
  endif
  set undodir=./.undo//
  set undodir+=~/.vim/undo//
  set undofile
endif


"--------------------------------------------------
" Syntastic
"--------------------------------------------------
let g:syntastic_check_on_open = 1
let g:syntastic_enable_signs = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_enable_highlighting = 1
let g:syntastic_mode_map = { 'mode': 'active',
                               \ 'active_filetypes': ['ruby', 'php', 'python', 'c', 'cpp'],
                               \ 'passive_filetypes': ['puppet'] }


"--------------------------------------------------
" Status Line - except for the first line, this is
" ignored by PowerLine.
"--------------------------------------------------
set laststatus=2
set statusline=%f             " Path to the file
set statusline+=%=            " Switch to the right side
set statusline+=%#warningmsg# " Syntastic stuff(next 3 lines)
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%l            " Current line
set statusline+=/             " Separator
set statusline+=%L            " Total lines


"--------------------------------------------------
" PowerLine
"--------------------------------------------------
let g:Powerline_symbols='compatible'


"--------------------------------------------------
" neocomplcache
"--------------------------------------------------

" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()


" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'

"--------------------------------------------------
" neosnippet
"--------------------------------------------------
imap <leader><TAB> <Plug>(neosnippet_expand_or_jump)
smap <leader><TAB> <Plug>(neosnippet_expand_or_jump)
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

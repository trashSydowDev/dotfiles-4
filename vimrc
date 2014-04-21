"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""" This file is mantained by tacla.yamada """"""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"------------------------------------------------------------------------------
" Plugins (Vundle)
"------------------------------------------------------------------------------
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle manages vundle
Bundle 'gmarik/vundle'
" Programming-specific plugins
Bundle 'tpope/vim-fugitive'
Bundle 'elzr/vim-json'
Bundle 'scrooloose/syntastic'
Bundle 'mattn/emmet-vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'gkz/vim-ls'
Bundle 'othree/javascript-libraries-syntax.vim'
Bundle 'digitaltoad/vim-jade'
Bundle 'nono/vim-handlebars'
Bundle 'heartsentwined/vim-emblem'
Bundle 'wavded/vim-stylus'
Bundle 'groenewege/vim-less'
Bundle 'tpope/vim-fireplace'
Bundle 'guns/vim-clojure-static'
Bundle 'derekwyatt/vim-scala'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/vimshell.vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'eagletmt/neco-ghc'
Bundle 'dag/vim2hs'
Bundle 'bitc/vim-hdevtools'
Bundle 'jnwhiteh/vim-golang'
" Easier editing plugins
Bundle 'Raimondi/delimitMate'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-surround'
Bundle 'godlygeek/tabular'
Bundle 'sjl/gundo.vim'
" UX plugins
Bundle 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
Bundle 'noahfrederick/Hemisu'
Bundle 'vim-scripts/CSApprox'
Bundle 'euclio/vim-nocturne'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'spolu/dwm.vim'
Bundle 'justincampbell/vim-eighties.git'
" Movement plugins
Bundle 'Lokaltog/vim-easymotion'
" Auto-complete and snippet plugins
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/neosnippet'
" File navigation and opening plugins
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'vim-scripts/sudo.vim'

filetype plugin indent on

"------------------------------------------------------------------------------
" Standard config
"------------------------------------------------------------------------------
" Looks
set ttyfast    " indicates we have a strong
               " terminal connection
set ttimeoutlen=50
syntax on
colorscheme Tomorrow-Night-Bright
" Override colorscheme bg so they look properly under any decent terminal -
" it's more of a hack than anything else
highlight Normal ctermbg=NONE
set cursorline
set cursorcolumn
" Show trailing spaces
set listchars=trail:.,tab:\|-
set list

" Look good on linux:
if has("unix")
  let s:uname = system("echo -n \"$(uname)\"")
  if !v:shell_error && s:uname == "Linux"
    set t_Co=256
    if $TERM =~ '256color'
      " Disable Background Color Erase (BCE) so that color schemes work
      " properly when Vim is used inside tmux and GNU screen.  See also
      " http://snk.tuxfamily.org/log/vim-256color-bce.html
      set t_ut=
    endif
  endif
endif

" Make vim use absolute numbers
set number
" Indentation, textwidth and colorcolumn
set autoindent smartindent
set textwidth=80
set colorcolumn=80
" Set tabs as 2 spaces - type :retab for setting a
" file's tabs to spaces
set expandtab
set shiftwidth=2
set tabstop=2
" Better overall tab key behaviour
set smarttab
" General
set hidden                   " edit multiple unsaved files at the
                             " same time
set ic                       " ignorecase in search
set complete=.,w,b,u,U,t,i,d
set completeopt-=preview
"set clipboard=unnamed        " yank and paste with the system clipboard
set noerrorbells
set wildmenu                 " better shell command managing
set pastetoggle=<F2>
set encoding=utf-8
set incsearch                " Highlight searches as they're typed
set hlsearch
set noesckeys
" I personally prefer this. But when it's
" convenient you can always change it with
" set nowrap
set wrap
set nocompatible
set foldlevel=99
set backspace=indent,eol,start
filetype plugin on

"------------------------------------------------------------------------------
" GUI
"------------------------------------------------------------------------------
set guifont=Monaco:h10
set guioptions=

"------------------------------------------------------------------------------
" Mappings
"------------------------------------------------------------------------------
let mapleader = ","

" Write RO files
cnoremap sudow w !sudo tee % >/dev/null

" Easily split and close windows
nnoremap <leader>ss :split<cr>
nnoremap <leader>vv :vsplit<cr>
nnoremap <leader>cc :close<cr>

" Easily move lines around:
nnoremap [e dd\|k\|P
nnoremap ]e dd\|p

" Easily check for and navigate errors
nnoremap <leader>ee :Errors<cr>
nnoremap <silent> <leader>en :lNext<cr>
nnoremap <silent> <leader>eN :lPrev<cr>

" Turn paste mode on or off
nnoremap <silent> [p :set paste<cr>
nnoremap <silent> ]p :set nopaste<cr>

" Toggle rainbow parentheses
nnoremap <silent> <leader>p :RainbowParenthesesToggle<cr>

" Quickly source or edit .vimrc:
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vs $MYVIMRC<cr>

" Repeat last substitution
nnoremap <leader>r :s<cr>

" Jump to definition or declaration (YCM)
nnoremap <leader>g :Ack<Space>

" Resize to textwidth
nnoremap <silent> <leader>tw :call TextWidthResize()<CR>

func! TextWidthResize()
  exec "vertical resize ".(&textwidth+4) 
  " The +4 is simply to account for the line numbers - solving this problem
  " better is a TODO
endfunc

" File type useful coding stuff:
augroup fileTypeMods
  autocmd!
  " Jade
  autocmd FileType jade set shiftwidth=2
  " Html
  autocmd FileType html set shiftwidth=2
  " Javascript
  autocmd FileType javascript set shiftwidth=2
  autocmd FileType javascript nnoremap <buffer> <leader>mk :w<CR>:!node %<cr>
  autocmd FileType javascript nnoremap <buffer> <leader>ts :w<CR>:!mocha -R spec -t 0 %<cr>
  " CoffeeScript
  autocmd FileType coffee set shiftwidth=2
  autocmd FileType coffee nnoremap <buffer> <leader>ms :w<CR>:CoffeeWatch<cr>
  autocmd FileType coffee nnoremap <buffer> <leader>mk :w<CR>:CoffeeRun<cr>
  autocmd FileType coffee nnoremap <buffer> <leader>ts :w<CR>:!mocha --compilers coffee:coffee-script/register -R spec -t 0 %<cr>
  " LiveScript
  autocmd FileType ls set shiftwidth=2
  autocmd FileType ls nnoremap <buffer> <leader>mk :LiveScriptCompile vert watch<CR>
  " CSS
  autocmd FileType css set shiftwidth=2
  " Python
  autocmd FileType python nnoremap <buffer> <leader>c I# <esc>j0
  autocmd FileType python nnoremap <buffer> <leader>mk :call InterpretPython()<CR>
  autocmd FileType python nnoremap <buffer> <leader>nk :w<CR>:!python %
  " Ruby
  autocmd FileType ruby nnoremap <buffer> <leader>mk :!rake<cr>
  " C
  autocmd FileType c nnoremap <buffer> <leader>mk :call CompileRunGcc()<CR>
  " Haskell
  autocmd FileType haskell nnoremap <buffer> <leader>mk :w<CR>:!runhaskell %<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mt :HdevtoolsType<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mi :HdevtoolsInfo<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mc :HdevtoolsClear<CR>
  autocmd FileType haskell set shiftwidth=2
  " VimScript
  autocmd FileType vim nnoremap <buffer> <leader>fl :call FillLine('-')<CR>
  " Git
  autocmd FileType gitcommit set spell
  autocmd FileType gitcommit set textwidth=72
  autocmd FileType gitcommit set colorcolumn=72,50
  autocmd FileType gitcommit colorscheme hemisu
augroup END

" Function - FillLine(str) (borrowed from - http://bit.ly/1g4Pi59)
func! FillLine(str)
  let tw = &textwidth - 1
  .s/[[:space:]]*$//
  let reps = (tw - col("$")) / len(a:str)
  if reps > 0
    .s/$/\=(' ').repeat(a:str, reps)/
  endif
endfunc

" Compile or interpret
func! CompileRunGcc()
  exec "w"
  exec "!gcc --std=c99 -Wall % -o %<.out; ./%<.out"
endfunc
func! InterpretPython()
  exec "w"
  exec "!python %"
endfunc

" Tabs
nnoremap <leader>tt :tab<Space>
nnoremap tt :tabnew<CR>
nnoremap tn :tabnext<CR>
nnoremap te :tabedit
nnoremap tc :tabclose<CR>
nnoremap tn :tabnext<CR>
nnoremap tp :tabprevious<CR>

" Tabularize
vnoremap <leader>w :Tabularize/
vnoremap <leader><leader>; :Tabularize/:\zs/l1r0<cr>
vnoremap <leader><leader><space> :Tabularize/\w\s\zs/l1r0<cr>

" Clear search
nnoremap <silent><Leader>/ :nohlsearch<CR>

" Make H and L go to the beggining and end of a
" line, respectively.
noremap H ^
noremap L $

" Open ctags definition in a new tab
noremap <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
" Open ctags definition in a vertical split - instead of a horizontal one
noremap <C-W><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" General tweaks:
onoremap p i(


"------------------------------------------------------------------------------
" Backup (without bloat)
"------------------------------------------------------------------------------
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

"------------------------------------------------------------------------------
" ZenCoding
"------------------------------------------------------------------------------
let g:user_emmet_leader_key = '<c-k>'

"------------------------------------------------------------------------------
" Rainbow Parentheses
"------------------------------------------------------------------------------
au Vimenter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"------------------------------------------------------------------------------
" EasyMotion
"------------------------------------------------------------------------------
let g:EasyMotion_keys = "fjdksla;qpwoeirutycmvnx.bz,"

"------------------------------------------------------------------------------
" Persistent Undo
"------------------------------------------------------------------------------
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


"------------------------------------------------------------------------------
" Syntastic
"------------------------------------------------------------------------------
let g:syntastic_check_on_open = 1
let g:syntastic_enable_signs = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_enable_highlighting = 1
let g:syntastic_mode_map = { 'mode': 'active',
                               \ 'active_filetypes':  ['javascript', 'ruby',
                               \                       'php', 'python', 'c', 
                               \                       'cpp', 'haskell'],
                               \ 'passive_filetypes': ['html', 'puppet', 'json'] }
let g:syntastic_javascript_checkers = ['jshint']


"------------------------------------------------------------------------------
" Status Line - except for the first line, this is
" ignored by PowerLine.
"------------------------------------------------------------------------------
set laststatus=2
set statusline=%f             " Path to the file
set statusline+=%=            " Switch to the right side
set statusline+=%#warningmsg# " Syntastic stuff(next 3 lines)
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%l            " Current line
set statusline+=/             " Separator
set statusline+=%L            " Total lines

"------------------------------------------------------------------------------
" PowerLine
"------------------------------------------------------------------------------
let g:Powerline_symbols='compatible'
let g:Powerline_colorscheme='solarized256'
let g:Powerline_stl_path_style='full'

"------------------------------------------------------------------------------
" Haskell Mode Vim
"------------------------------------------------------------------------------
let g:haddock_browser = 'open'
let g:haddock_browser_callformat = '%s %s'

"------------------------------------------------------------------------------
" vim-eighties
"------------------------------------------------------------------------------
let g:eighties_enabled = 1
let g:eighties_minimum_width = &textwidth
let g:eighties_extra_width = 4
let g:eighties_compute = 0

"------------------------------------------------------------------------------
" ctrlp
"------------------------------------------------------------------------------
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|bower_components\|vendor'

"------------------------------------------------------------------------------
" neocomplcache
"------------------------------------------------------------------------------
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

"------------------------------------------------------------------------------
" neosnippet
"------------------------------------------------------------------------------
imap <leader><TAB> <Plug>(neosnippet_expand_or_jump)
smap <leader><TAB> <Plug>(neosnippet_expand_or_jump)
if has('conceal')
  set conceallevel=2 concealcursor=i
endif
let g:neosnippet#snippets_directory='~/dotfiles/vim/snippets'

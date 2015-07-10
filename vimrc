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
Bundle 'zah/nimrod.vim'
Bundle 'mintplant/vim-literate-coffeescript'
Bundle 'gkz/vim-ls'
Bundle 'othree/javascript-libraries-syntax.vim'
Bundle 'digitaltoad/vim-jade'
Bundle 'lambdatoast/elm.vim'
Bundle 'nono/vim-handlebars'
Bundle 'heartsentwined/vim-emblem'
Bundle 'dbakker/vim-lint'
Bundle 'elixir-lang/vim-elixir'
Bundle 'wavded/vim-stylus'
Bundle 'groenewege/vim-less'
Bundle 'tpope/vim-fireplace'
Bundle 'guns/vim-clojure-static'
Bundle 'guns/vim-sexp'
Bundle 'tpope/vim-classpath'
Bundle 'derekwyatt/vim-scala'
Bundle 'Shougo/unite.vim'
Bundle 'osyo-manga/unite-quickfix'
Bundle 'eagletmt/unite-haddock'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/vimshell.vim'
Bundle 'wting/rust.vim'
Bundle 'eagletmt/neco-ghc'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'dag/vim2hs'
Bundle 'jpalardy/vim-slime'
Bundle 'bitc/vim-hdevtools'
Bundle 'fatih/vim-go'
Bundle 'honza/dockerfile.vim'
Bundle 'leafgarland/typescript-vim'
Bundle 'dart-lang/dart-vim-plugin'
" Easier editing plugins
Bundle 'Raimondi/delimitMate'
Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-surround'
Bundle 'godlygeek/tabular'
Bundle 'majutsushi/tagbar'
Bundle 'sjl/gundo.vim'
" UX plugins
Bundle 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
Bundle 'chriskempson/base16-vim'
Bundle 'noahfrederick/Hemisu'
Bundle 'vim-scripts/CSApprox'
Bundle 'euclio/vim-nocturne'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kien/rainbow_parentheses.vim'
" Movement plugins
Bundle 'Lokaltog/vim-easymotion'
" Auto-complete and snippet plugins
Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/neosnippet'
Bundle 'Shougo/neosnippet-snippets'
" File navigation and opening plugins
Bundle 'mileszs/ack.vim'
Bundle 'vim-scripts/sudo.vim'
" Other
Bundle 'szw/vim-g'
Bundle 'yuratomo/w3m.vim'

filetype plugin indent on

"------------------------------------------------------------------------------
" Standard config
"------------------------------------------------------------------------------
" Looks
set ttyfast    " indicates we have a strong
               " terminal connection
set ttimeoutlen=10
set tenc=utf8
" Minimize the escape delay
if ! has('gui_running')
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif
syntax on
let base16colorspace=256  " Access colors present in 256 colorspace
set background=dark
colorscheme base16-summerfruit
" Override colorscheme bg so they look properly under any decent terminal -
" it's more of a hack than anything else
"highlight Normal ctermbg=NONE
set cursorline
set cursorcolumn
" Show trailing spaces
set listchars=trail:.,tab:--
set tags=./tags,tags,codex.tags
set list
set shell=/usr/local/bin/zsh
let $PATH.=':/Users/adam/.cabal/bin'
let $PATH.=':/Users/adam/Library/Haskell/bin'

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
set ignorecase               " be case-insensitive in search
set smartcase                " but be case-sensitive with any upper-case chars
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
set foldmethod=indent
set backspace=indent,eol,start
filetype plugin on

"------------------------------------------------------------------------------
" GUI
"------------------------------------------------------------------------------
set guifont=Monaco:h11
set guioptions=
if $TERM =~ 'italic'
  highlight Comment cterm=italic
endif

"------------------------------------------------------------------------------
" Mappings
"------------------------------------------------------------------------------
let mapleader = ","

" Write RO files
cnoremap sudow w !sudo tee % >/dev/null

" Increment numbers
nnoremap <leader>aa <C-a>

" Easily move lines around:
nnoremap [e dd\|k\|P
nnoremap ]e dd\|p

" Easily check for and navigate errors
nnoremap <leader>ee :Errors<cr>
nnoremap <silent> <leader>en :lnext<cr>
nnoremap <silent> <leader>eN :lprevious<cr>

" Turn paste mode on or off
nnoremap <silent> [p :set paste<cr>
nnoremap <silent> ]p :set nopaste<cr>

" Toggle rainbow parentheses
nnoremap <silent> <leader>p :RainbowParenthesesToggle<cr>

" Quickly source or edit .vimrc:
"nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vs $MYVIMRC<cr>

" Repeat last substitution
nnoremap <leader>r :s<cr>

" Jump to definition or declaration (YCM)
nnoremap <leader>g :Ack<Space>

vnoremap <leader>if :Googlef<cr>
vnoremap <leader>ig :Google<cr>
nnoremap <leader>ig :Google<space>

" Resize to textwidth
nnoremap <silent> <leader>ty :SyntasticToggleMode<cr>
nnoremap <silent> <leader>sc :SyntasticCheck<cr>

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
  autocmd FileType javascript nnoremap <buffer> <leader>mj :w<CR>:!node --harmony %<cr>
  autocmd FileType javascript nnoremap <buffer> <leader>ts :w<CR>:!mocha -R spec -t 10000 %<cr>
  autocmd FileType javascript nnoremap <buffer> <leader>co :w<CR>:!mocha --require blanket -R html-cov % > cov.html; open cov.html<CR>
  " CoffeeScript
  autocmd FileType coffee set shiftwidth=2
  autocmd FileType coffee nnoremap <buffer> <leader>ms :w<CR>:CoffeeWatch<cr>
  autocmd FileType coffee nnoremap <buffer> <leader>mk :w<CR>:CoffeeRun<cr>
  autocmd FileType coffee nnoremap <buffer> <leader>ts :w<CR>:!mocha --compilers coffee:coffee-script/register -R spec -t 10000 %<cr>
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
  autocmd FileType haskell nnoremap <buffer> <leader>md :Unite haddock<cr>
  autocmd FileType haskell nnoremap <buffer> <leader>mf :Unite hoogle<cr>
  autocmd FileType haskell nnoremap <buffer> <leader>mk :w<CR>:!runhaskell %<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mt :HdevtoolsType<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mi :HdevtoolsInfo<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mc :HdevtoolsClear<CR>
  autocmd FileType haskell nnoremap <buffer> <leader>mp :PointFree<CR>
  autocmd FileType haskell set shiftwidth=2
  autocmd FileType haskell nmap <buffer> <C-C><C-k> :set ft=haskell.script<cr><C-c><C-c>:set ft=haskell<cr>
  autocmd FileType haskell nmap <buffer> <C-C><C-r> :set ft=haskell.script<cr>:SlimeSend1 :reload<cr>:set ft=haskell<cr>
  " Golang
  autocmd FileType go nnoremap <buffer> <leader>mk :w<CR>:!go run %<CR>
  " VimScript
  autocmd FileType vim nnoremap <buffer> <leader>fl :call FillLine('-')<CR>
  " Git
  autocmd FileType gitcommit set spell
  autocmd FileType gitcommit set textwidth=72
  autocmd FileType gitcommit set colorcolumn=72,50
  " Dart
  autocmd FileType dart nnoremap <buffer> <leader>mk :!dart %<CR>
  " DLang
  autocmd FileType d nnoremap <buffer> <leader>mk :!rdmd %<CR>
  autocmd FileType d nnoremap <buffer> <leader>ts :!rdmd -unittest %<CR>
augroup END

nnoremap <leader>mo <C-w>s<C-w>J:VimShell<cr><esc>:resize 10<cr>i

" Support all markdown extensions
au BufNewFile,BufRead *.markdown,*.mdown,*.mkd,*.mkdn,*.md set filetype=markdown
au BufNewFile,BufRead *.hss set filetype=haskell.script

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
  exec "!/usr/local/bin/python %"
endfunc

" Tabs
nnoremap tt :tabnew<CR>
nnoremap tn :tabnext<CR>
nnoremap tp :tabprevious<CR>
nnoremap tc :tabclose<CR>

" Tabularize
vnoremap <leader>w :Tabularize/
vnoremap <leader><leader>; :Tabularize/:\zs/l1r0<cr>
vnoremap <leader><leader><space> :Tabularize/\w\s\zs/l1r0<cr>

" NERDTree
nnoremap <leader>nt :NERDTreeTabsToggle<cr>
let g:nerdtree_tabs_open_on_gui_startup = 0

" Tagbar
nnoremap <leader>. :Tagbar<cr>
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

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
" vim2hs
"------------------------------------------------------------------------------
let g:haskell_conceal_enumerations = 0

"------------------------------------------------------------------------------
" Backup (without bloat)
"------------------------------------------------------------------------------
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

"------------------------------------------------------------------------------
" ZenCoding
"------------------------------------------------------------------------------
let g:user_emmet_leader_key = '<C-k>'

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
let g:syntastic_check_on_open = 0
let g:syntastic_enable_signs = 1
let g:syntastic_enable_balloons = 1
let g:syntastic_enable_highlighting = 1
let g:syntastic_mode_map = { 'mode': 'active',
                               \ 'active_filetypes':  ['javascript', 'ruby',
                               \                       'php', 'python', 'c',
                               \                       'cpp'],
                               \ 'passive_filetypes': ['html', 'puppet', 'json',
                               \                       'dart'] }
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_coffee_coffeelint_args = "--csv --file ./coffeelint.json"
let g:syntastic_aggregate_errors = 1

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
" ctrlp
"------------------------------------------------------------------------------
nnoremap <C-p> :Unite file_rec<cr>

"------------------------------------------------------------------------------
" slime
"------------------------------------------------------------------------------
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()

let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"------------------------------------------------------------------------------
" neosnippet
"------------------------------------------------------------------------------
imap <C-l> <Plug>(neosnippet_expand_or_jump)
smap <C-l> <Plug>(neosnippet_expand_or_jump)
xmap <C-l> <Plug>(neosnippet_expand_target)
if has('conceal')
  set conceallevel=2 concealcursor=i
endif
"let g:neosnippet#snippets_directory='~/dotfiles/vim/snippets'

"------------------------------------------------------------------------------
" VimShell
"------------------------------------------------------------------------------
let g:vimshell_user_prompt = 'getcwd()'
let g:vimshell_prompt =  '$ '

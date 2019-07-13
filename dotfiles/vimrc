
""" settings """
""""""""""""""""
filetype on
filetype plugin on
filetype indent on
set nocompatible

set encoding=utf8
set termencoding=utf8

if &term == 'xterm' || &term == 'screen'
  set t_Co=256
endif

syntax on

set cursorline " highlight current line and line number
set expandtab " spaces instead of tabs
set laststatus=2 " Always show the status line
set number " line numbers
set showcmd " Show commands as they're typed
set showmatch " Show matching brackets when text indicator is over them

" when a search pattern is all lowercase, it is case-insensitive
" when a search pattern contains a capital, it is case-sensitive
set ignorecase
set smartcase
set hlsearch " highlight search results
set incsearch " execute search after each character is typed

" Backspace can delete newlines
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" unlimited characters per line
" set lbr
" set tw=0

" filetype-specific indentation
autocmd FileType clojure,ruby,css,html setlocal ts=2 sts=2 sw=2
autocmd FileType python,java,javascript setlocal ts=4 sts=4 sw=4

" filetype settings
autocmd FileType clojure setlocal lispwords+=describe,it,context,around
autocmd FileType clojure setlocal wildignore+=target/**/*
autocmd BufNewFile,BufRead *.hiccup set filetype=clojure
autocmd BufNewFile,BufRead *.cljs set filetype=clojure
autocmd BufNewFile,BufRead *.ejs set filetype=eruby.html
autocmd BufNewFile,BufRead *.erb set filetype=eruby.html
autocmd BufNewFile,BufRead *.json set filetype=javascript

" config files with ruby syntax
autocmd BufEnter,BufNewFile,BufRead Vagrantfile,Berksfile,Gemfile set filetype=ruby

" pressing tab in command shows options
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,node_modules,tmp,coverage
if has("win16") || has("win32")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
    set wildignore+=.git\*,.hg\*,.svn\*
endif




""" mappings """
""""""""""""""""
nnoremap ; :

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Remap x to delete instead of cut
noremap x "_dl

" set * to not automatically jump to next word
nnoremap * ma*`a

" unmap ex mode, F1 help
nnoremap Q <nop>
nnoremap <C-a> <nop>
nmap <F1> <nop>
imap <F1> <nop>



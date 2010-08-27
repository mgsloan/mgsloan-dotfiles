noremap <silent> <C-n> :tabnext<CR>
noremap <silent> <C-p> :tabprevious<CR>
nnoremap <silent> <t>  :tabnew
nnoremap <silent> <Home> i <Esc>r
nnoremap <silent> <End> a <Esc>r
nnoremap <silent> <C-W>o <Nop>

noremap  <C-D> <Esc>:NERDTreeToggle<CR>
noremap! <C-D> <Esc>:NERDTreeToggle<CR>

"more logical motion
nnoremap <silent> k gk
nnoremap <silent> j gj
inoremap <silent> <Up> <Esc>gka
inoremap <silent> <Down> <Esc>gja

"newlines in normal mode
nnoremap <silent> zj o<Esc>
nnoremap <silent> zk O<Esc>

nnoremap <space> <C-F>
nnoremap <Shift><space> <C-B>

" Swap ; and :  Convenient.
nnoremap ; :
nnoremap : ;

nnoremap w W;
nnoremap W w;
nnoremap b B;
nnoremap B b;

inoremap ii <Esc>
inoremap jj <Esc>
nnoremap JJJJ <Nop>

vnoremap <Enter> <Esc>
inoremap <Enter> <Esc>
inoremap <Enter><Enter> <Enter>
nnoremap <Enter> i
vnoremap <Shift><Enter> <Enter>
inoremap <Shift><Enter> <Enter>

set gfn=Inconsolata\ Medium\ 9

set autoindent
set expandtab
set smarttab

set shiftwidth=2
set softtabstop=2
set tabstop=8
set textwidth=75
set formatoptions=cq

if version >= 700
   set spl=en spell
   set nospell
endif

"set wildmenu
"set wildmenu=list:longest,full

set mouse=a
set backspace=2
set number
set ignorecase
set smartcase

set incsearch
set hlsearch

" Linux clipboard
let g:clipbrdDefaultReg = '+'

" closed tabs = closed buffers
set nohidden

" highlight matching paren
highlight  MatchParen ctermbg=4

set noerrorbells
set t_vb=

set autochdir
set autoread

"highlight lines over 80
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

set lazyredraw

"Removes Vi compatibility stuff
set nocompatible
set nobackup
syntax on

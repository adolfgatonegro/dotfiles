" Don't try to be vi compatible
set nocompatible

" Turn on syntax highlighting
syntax on

" Show line numbers
set number

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
:  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
:augroup END

" Show file stats
set ruler

" Set decent tabs
set tabstop=4
set shiftwidth=4

" Always show status line
set laststatus=2

" Customise the status line
set statusline=
set statusline+=%1*
set statusline+=\ %F
set statusline+=\ %m
set statusline+=%r
set statusline+=\ %=
set statusline+=%2*
set statusline+=\ %y 
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\ %l
set statusline+=:
set statusline+=%L
set statusline+=\ %c
set statusline+=\ %p%%
set statusline+=\ 
hi User1 cterm=bold ctermbg=89 ctermfg=15
hi User2 cterm=bold ctermbg=240 ctermfg=15

" Something to keep in mind for later: https://pastebin.com/qWRQVzES

" Ask about saving instead of failing due to unsaved changes
set confirm

" Visual bell instead of beeping
set visualbell
set t_vb=

" Auto indent new line when unknown filetype
set autoindent

" Use case insensitive search except when using caps
set ignorecase
set smartcase

" Enable cursorline in insert mode for better visibility 
hi CursorLine ctermbg=238 cterm=none
au InsertEnter * set cursorline
au InsertLeave * set nocursorline

" Different cursors for Normal, Insert, and Replace modes
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

set guifont-=UbuntuMono\ Nerd\ Font:h12
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar


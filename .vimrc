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


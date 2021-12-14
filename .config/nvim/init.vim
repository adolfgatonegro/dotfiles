"
" ⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⣠⣄⠀⠀⠀⠀⠀⠀
"⠀⠀⠀⠀⠀⠀⡠⠖⣿⣧⢻⣿⢿⣷⣤⡀⠄⠀⠀⠀init.vim
"⠀⠀⠀⠀⣠⠊⠀⠂⣿⡏⣾⣿⠈⢻⠟⠉⠀⠀⠀⠀--------
"⠀⠀⠀⢸⣿⠀⠀⢰⣿⣷⢻⣿⠴⣿⣷⣦⡀⠀⠀⠀Configuration file for Neovim.
"⠀⠀⠀⣿⣿⡄⠀⡇⣿⣧⣿⣿⠀⠈⢿⣿⡇⠀⠀⠀
"⠀⠀⠀⠈⢿⣿⣦⣱⠃⠀⣿⠟⠁⠀⠀⡿⠃⠀⠀⠀Requires Vundle and Vifm.
"⠀⠀⠀⠀⠀⠙⢿⣿⣿⣶⣧⣤⣤⡤⠚⠁⠀⠀⠀⠀
"⠀⠀⠀⠀⠀⠀⠀⠌⠉⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀
"
" -----------------------------------------------------------------------------  

set nocompatible			" be iMproved, required
filetype off				" required
filetype plugin on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Call Vundle plugin manager
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
	Plugin 'VundleVim/Vundle.vim'		" Vundle
	Plugin 'itchyny/lightline.vim'		" Lightline
	Plugin 'vifm/vifm.vim'				" Vifm
	Plugin 'ap/vim-css-color'			" CSS colour previews
	Plugin 'mattn/emmet-vim'			" Emmet
	Plugin 'vimwiki/vimwiki'			" VimWiki
	Plugin 'neoclide/coc.nvim', {'branch': 'release'} " intellisense engine
	Plugin 'jiangmiao/auto-pairs'		" auto-close braces and scopes
	Plugin 'tpope/vim-commentary'
	Plugin 'Chiel92/vim-autoformat'
	Plugin 'tmhedberg/SimpylFold'
	Plugin 'lervag/vimtex'
	" Plugin 'junegunn/goyo.vim'	
call vundle#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256				" Set 256 colour support
set termguicolors			" Set termguicolors
set number relativenumber	" Display line numbers
set nobackup				" Don't make auto-backups
set confirm					" Ask about saving changes instead of yelling at me
syntax enable

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings and remapping
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set leader to º
let mapleader = "º"
" Remap jk to ESC 
inoremap <silent> jj <Esc>
" Shortcut for faster save and quit
nnoremap <silent> <leader>w :update<CR>
" Saves the file if modified and quit
nnoremap <silent> <leader>q :x<CR>
" Quit all opened buffers
nnoremap <silent> <leader>Q :qa<CR>
" Yank/put to/from system clipboard
vnoremap <silent> <C-Y> "+y<CR>
nnoremap <silent> <C-P> "+p<CR>
" Toggle search highlight
nnoremap <silent><expr> <Leader>hl (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"
" Jump to matching pairs easily in normal mode
nnoremap <Tab> %
" Remap Emmet autocompletion to Ctrl+Z
let g:user_emmet_leader_key='<C-Z>'
" SimpylFold config
let g:SimpylFold_docstring_preview = 1
" coc shortcuts
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
" vim-autoformat
noremap <F3> :Autoformat<CR>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vifm
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>vf :Vifm<CR>
map <Leader>vs :VsplitVifm<CR>
map <Leader>sp :SplitVifm<CR>
map <Leader>dv :DiffVifm<CR>
map <Leader>tv :TabVifm<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VimWiki
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vimwiki_list = [{'path': '~/Documents/wiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Dynamic line numbers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup numbertoggle
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
	autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END

augroup term_settings
	autocmd!
    " Do not use number and relative number for terminal inside nvim
    autocmd TermOpen * setlocal norelativenumber nonumber
    " Go to insert mode by default to start typing command
    autocmd TermOpen * startinsert
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Status line
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
let g:lightline = {
		\ 'colorscheme': 'NeonGatoLightline',
		\ }
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Python
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup do_python_setup
	autocmd! filetype python set expandtab
	autocmd! filetype python nnoremap <F5> :Autoformat<CR>:w<CR>:sp<CR>:term python %<CR>
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vimtex
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
" let g:vimtex_view_general_viewer = 'zathura'
" let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'
" let g:vimtex_view_general_options_latexmk = '--unique'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text, tabs, indentation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set textwidth for text file types
augroup text_file_width
    autocmd!
	autocmd BufNewFile,BufRead *.md,*.MD,*.markdown,*.txt,*tex setlocal textwidth=100
augroup END

set fo+=t					" Set format options to include text width
set tabstop=4				" Set tab size to 4 spaces
set softtabstop=4
set shiftwidth=4
set autoindent
set backspace=indent,eol,start

" Return to last edit position when opening a file
augroup resume_edit_position
    autocmd!
    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
        \ | execute "normal! g`\"zvzz"
        \ | endif
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Splits and tabs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

" Remap split navigation to Ctrl + HJKL
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Remap split size adjustment to Ctrl + Arrow keys
noremap <silent> <C-Left> :vertical resize +3<CR>
noremap <silent> <C-Right> :vertical resize -3<CR>
noremap <silent> <C-Up> :resize +3<CR>
noremap <silent> <C-Down> :resize -3<CR>

" Switch splits from vertical to horizontal and viceversa
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Fonts and colours
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark
colorscheme NeonGato

" GUI settings for Neovim-Qt
set guifont=FiraCode\ Nerd\ Font\ Mono:h10:b " Set font for GUI
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mouse scrolling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set mouse=a

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Misc settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set noshowmode
set ignorecase				" Case insensitive search terms
set smartcase				" unless there's an uppercase character


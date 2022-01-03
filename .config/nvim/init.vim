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

source $XDG_CONFIG_HOME/nvim/xdg.vim

filetype off				" required
filetype plugin on
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Call Vundle plugin manager
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('$XDG_CONFIG_HOME/nvim/plugs')
	Plug 'itchyny/lightline.vim'
	Plug 'vifm/vifm.vim'
	Plug 'ap/vim-css-color'
	Plug 'mattn/emmet-vim'
	Plug 'vimwiki/vimwiki'
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'jiangmiao/auto-pairs'
	Plug 'tpope/vim-commentary'
	Plug 'Chiel92/vim-autoformat'
	Plug 'tmhedberg/SimpylFold'
	Plug 'lervag/vimtex'
	Plug 'junegunn/goyo.vim'	
call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256				" Set 256 colour support
set termguicolors			" Set termguicolors
set number					" Display line numbers
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
nnoremap <silent> <C-w> :update<CR>
" Saves the file if modified and quit
nnoremap <silent> <leader>q :x<CR>
" Quit all opened buffers
nnoremap <silent> <leader>Q :qa<CR>
" Toggle Goyo
nnoremap <silent> <leader>G :Goyo<CR>
" Yank/put to/from system clipboard
vnoremap <silent> <C-Y> "+y<CR>
nnoremap <silent> <C-P> "+p<CR>
" Toggle search highlight
nnoremap <silent><expr> <Leader>hl (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"
" Jump to matching pairs easily in normal mode
nnoremap <Tab> %
" Remap Emmet autocompletion to Ctrl+Z
imap ,, <C-y>,
" let g:user_emmet_leader_key='<C-Z>'
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
let g:vimwiki_global_ext = 0
let g:vimwiki_list = [{'path': '~/repos/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]
" Goyo
let g:goyo_width = 84

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Dynamic line numbers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text, tabs, indentation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set textwidth for text file types
augroup text_file_width
    autocmd!
	autocmd BufNewFile,BufRead *.md,*.MD,*.markdown,*.txt,*tex setlocal textwidth=80
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
set guifont=FiraCode\ Nerd\ Font\ Mono:h9 " Set font for GUI
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


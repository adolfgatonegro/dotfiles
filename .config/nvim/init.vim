"
"            /o   ooooo          
"         oooooo oooooooo+       
"      /.  o ooo oooo ooooo\     
"    oo    /oooo ooo    \           NEOVIM CONFIGURATION FILE
"  .oo     ( ooo ooo+oooooo         init.vim
"  ooo     ooooo&ooo   oooooo       ....................
"  oooo    &oooooooo     oooo       Gatonegro
"   ooooo, / (   oooo.    /oo       https://gatoneg.ro/
"     ooooooo    o        oo     
"       ooooooooooo&//ooo(       
"          ooooooooooo/         
"
"  Nothing fancy, just a standard Nvim config file...
"
"  (I don't know why I keep doing this 😹)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible			" be iMproved, required
filetype off				" required
filetype plugin on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Call Vundle plugin manager
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
		Plugin 'VundleVim/Vundle.vim'						" Vundle
		Plugin 'itchyny/lightline.vim'						" Lightline
		Plugin 'vifm/vifm.vim'								" Vifm
		Plugin 'ap/vim-css-color'							" CSS colour previews
		Plugin 'tpope/vim-commentary'						" Toggle comments
		Plugin 'tpope/vim-surround'							" Handle/edit surrounding characters
		Plugin 'mattn/emmet-vim'							" Emmet
		Plugin 'vimwiki/vimwiki'							" VimWiki
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
" Remap ESC to ii
:imap ii <Esc>

" Remap Emmet autocompletion to Ctrl+Z
let g:user_emmet_leader_key='<C-Z>'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vifm
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>vv :Vifm<CR>
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
:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
:  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
:augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Status line
" Something to keep in mind for later: https://pastebin.com/qWRQVzES
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2

let g:lightline = {
		\ 'colorscheme': 'powerlineish',
		\ }

"set statusline=
"set statusline+=%1*
"set statusline+=\ %{StatuslineMode()}
"set statusline+=\ 
"set statusline+=%2*
"set statusline+=\ %F
"set statusline+=\ %r
"set statusline+=%m
"set statusline+=%=
"set statusline+=%2*
"set statusline+=\ %y
"set statusline+=\ | 
"set statusline+=\ %{&ff}
"set statusline+=\ %{strlen(&fenc)?&fenc:'none'}
"set statusline+=\ %P
"set statusline+=\ %L
"set statusline+=\ 
"set statusline+=%1*
"set statusline+=\ %l
"set statusline+=:
"set statusline+=%c
"set statusline+=\ 
"hi User1 cterm=bold guifg=#dfdfdf ctermfg=15 guibg=#e60099 ctermbg=89
"hi User2 cterm=bold guifg=#dfdfdf ctermfg=15 guibg=#222222 ctermbg=235
"
"function! StatuslineMode()
"  let l:mode=mode()
"  if l:mode==#"n"
"    return "NORMAL"
"  elseif l:mode==?"Vs"
"    return "V-BLOCK"
"  elseif l:mode==?"v"
"    return "VISUAL"
"  elseif l:mode==#"i"
"    return "INSERT"
"  elseif l:mode==#"R"
"    return "REPLACE"
"  elseif l:mode==?"s"
"    return "SELECT"
"  elseif l:mode==#"t"
"    return "TERMINAL"
"  elseif l:mode==#"c"
"    return "COMMAND"
"  elseif l:mode==#"!"
"    return "SHELL"
"  endif
"endfunction


" Something to keep in mind for later: 
" https://pastebin.com/qWRQVzES

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text, tabs, indentation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tabstop=4				" Set tab size to 4 spaces
set shiftwidth=4
set smarttab				" Use smart tabs
set autoindent

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
"colorscheme darkfrost
set guifont=UbuntuMono\ Nerd\ Font\ Mono:h11	" Set font for GUI

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


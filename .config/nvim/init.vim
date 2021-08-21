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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Call Vundle plugin manager
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
		Plugin 'VundleVim/Vundle.vim'						" Vundle
		Plugin 'itchyny/lightline.vim'						" Lightline
		Plugin 'preservim/nerdtree'							" NERDTree
		Plugin 'ryanoasis/vim-devicons'						" Icons for NT
		Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'	" Syntax highlighting for NT
		Plugin 'ap/vim-css-color'							" CSS colour previews
call vundle#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256				" Set 256 colour support
set termguicolors
set number relativenumber	" Display line numbers
set nobackup				" Don't make auto-backups
set confirm					" Ask about saving changes instead of yelling at me
syntax enable

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings and remapping
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap ESC to ii
:imap ii <Esc>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = ''
let g:NERDTreeDirArrowCollapsible = ''
let NERDTreeShowLineNumbers=1
let NERDTreeShowHidden=1
let NERDTreeMinimalUI = 1
let g:NERDTreeWinSize=38

" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif

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
set smarttab				" Use smart tabs

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


set nocompatible
set textwidth=82
set backspace=indent,eol,start

set autoindent
set tabstop=2
set expandtab
set shiftwidth=2
set shiftround
set hlsearch

set matchpairs+=<:>
"let loaded_matchparen=1

"moving through open windows
map <C-j> <C-W>j<C-W>_
map <C-k> <C-W>k<C-W>_
map <C-h> <C-W>h<C-W>_
map <C-m> <C-W>l<C-W>_

call plug#begin('~/.vim/plugged')
Plug 'hzchirs/vim-material'
Plug 'tpope/vim-commentary'
Plug 'pangloss/vim-javascript'
Plug 'storyn26383/vim-vue'
Plug 'elixir-editors/vim-elixir'
Plug 'preservim/nerdtree'
Plug 'Chiel92/vim-autoformat'
Plug 'morhetz/gruvbox'
Plug 'junegunn/vim-easy-align'
Plug 'jiangmiao/auto-pairs'
call plug#end()

syntax on
set termguicolors
set background=dark
colorscheme gruvbox

set mouse=a
set foldcolumn=0

"set cursorline
"hi CursorLine cterm=none

set guioptions-=T
set guioptions-=r

map <C-n> :NERDTreeToggle<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)


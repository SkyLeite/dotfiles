" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')

Plug 'Quramy/tsuquyomi'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/denite.nvim'
Plug 'scrooloose/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'nightsense/cosmic_latte'
Plug 'ervandew/supertab'
Plug 'vim-scripts/Rename2'
Plug 'jiangmiao/auto-pairs'

call plug#end()

map <C-n> :NERDTreeToggle<CR>
map <C-l> :exec &nu==&rnu? "se nu!" : "se rnu!"<CR>
nnoremap <esc> :noh<return><esc>

set wildignore+=*/tmp/*,*.swp,*.zip,node_modules,.git
set background=dark
set relativenumber
set expandtab
set tabstop=4
set shiftwidth=4

colorscheme cosmic_latte

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:deoplete#enable_at_startup = 1
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:SuperTabDefaultCompletionType = "<c-n>"


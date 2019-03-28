" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Rust / Cargo garbage
function! BuildComposer(info)
  if a:info.status != 'unchanged' || a:info.force
    if has('nvim')
      !cargo build --release
    else
      !cargo build --release --no-default-features --features json-rpc
    endif
  endif
endfunction

call plug#begin('~/.local/share/nvim/plugged')

Plug 'mhartington/nvim-typescript', {'do': './install.sh'}
Plug 'HerringtonDarkholme/yats.vim'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/denite.nvim'
Plug 'scrooloose/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'nightsense/cosmic_latte'
Plug 'ervandew/supertab'
Plug 'vim-scripts/Rename2'
Plug 'jiangmiao/auto-pairs'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'Shougo/echodoc.vim'
Plug 'euclio/vim-markdown-composer', { 'do': function('BuildComposer') }
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

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
set completeopt=menu
set cmdheight=2

" colorscheme cosmic_latte
au ColorScheme * hi Normal ctermbg=none guibg=none
au ColorScheme myspecialcolors hi Normal ctermbg=red guibg=red

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:deoplete#enable_at_startup = 1
let g:deoplete#ignore_case = 1
let g:deoplete#completeopt = 0
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:mkdp_auto_start = 1
let g:mkdp_auto_close = 1


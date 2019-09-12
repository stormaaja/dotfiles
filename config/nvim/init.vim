call plug#begin('~/.local/share/nvim/plugged')
" Theme
Plug 'mhartington/oceanic-next'
" Editor config
Plug 'editorconfig/editorconfig-vim'

" Clojure

" Acid Clojure plugin https://github.com/clojure-vim/acid.nvim
" Plug 'clojure-vim/acid.nvim'

" Rainbow (parenthesis) https://github.com/luochen1990/rainbow
" Plug 'luochen1990/rainbow'

" Plug 'tpope/vim-surround'

" Plug '~/projects/nvim-parlint'

" deoplete.nvim (Completion
" if has('nvim')
"  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif

" clj-async.nvim (Completion)
" Plug 'clojure-vim/async-clj-omni'

" Parinfer (parenthesis)
" Plug 'snoe/nvim-parinfer.js'

" Indent guides
Plug 'Yggdroot/indentLine'

" Initialize plugin system
call plug#end()

" Enable Rainbow plugin
let g:rainbow_active = 1

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" clj-async-nvim Completion
let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

if (has("termguicolors"))
 set termguicolors
endif

" Theme
syntax enable
set background=dark
let g:impact_transbg=1
colorscheme OceanicNext

" Indent guide color
"let g:indentLine_setColors = 0

" Vim
"let g:indentLine_color_term = 239

" GVim
let g:indentLine_color_gui = '#343D46'

" Tab and indent width
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab

" Real tab for Makefile
autocmd FileType make setlocal noexpandtab

" Remove conceal (and IndentLine) for Latex-files
autocmd BufNewFile,BufRead *.tex IndentLinesDisable

" Invisible characters (ttf-droid)
set listchars=tab:▸\ ,eol:¬,trail:·,nbsp:·,extends:>,precedes:<
set list

" Other
set number
set colorcolumn=80
"
syntax on
filetype plugin indent on

" Set sync with X clipboard
set clipboard+=unnamedplus

" Format Clojure code
"nnoremap fc :%!parlinter --stdin --trim<Enter>
"nnoremap ff va):!zb<Enter>
"nnoremap ff va):!parlinter --stdin --trim<Enter>
" vnoremap fc :'<,'>!zb<Enter>

nnoremap fc :%call Parlint()<Enter>
nnoremap ff va):call Parlint()<Enter>
vnoremap ff :'<,'>call Parlint()<Enter>

nnoremap fg vipgq<Enter>
"vnoremap ff :'<,'>!parlinter --stdin --trim<Enter>
"nnoremap ff va):!parlinter --stdin --trim<Enter>
"nnoremap fc :%!parlinter --stdin --trim<Enter>

" Disable arrow keys in normal mode
" nnoremap <Up> <Nop>
" nnoremap <Down> <Nop>
" nnoremap <Left> <Nop>
" nnoremap <Rigth> <Nop>

" source ~/projects/nvim-parlint/parlint.vim

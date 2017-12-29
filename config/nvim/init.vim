call plug#begin('~/.vim/plugged')
" Theme
Plug 'mhartington/oceanic-next'
" Editor config
Plug 'editorconfig/editorconfig-vim'

" Clojure

" Acid Clojure plugin https://github.com/clojure-vim/acid.nvim
Plug 'clojure-vim/acid.nvim'

" Rainbow (parenthesis) https://github.com/luochen1990/rainbow
Plug 'luochen1990/rainbow'

" deoplete.nvim (Completion
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" clj-async.nvim (Completion)
Plug 'clojure-vim/async-clj-omni'

" Parinfer (parenthesis)
" Plug 'snoe/nvim-parinfer.js'

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

" Tab and indent width
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab

" Real tab for Makefile
autocmd FileType make setlocal noexpandtab

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
nnoremap fc :%!zb<Enter>
nnoremap ff va):!zb<Enter>
vnoremap fc :'<,'>!zb<Enter>


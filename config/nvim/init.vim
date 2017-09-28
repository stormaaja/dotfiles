" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" A simple, easy-to-use Vim alignment plugin.
" https://github.com/junegunn/vim-easy-align
" Plug 'junegunn/vim-easy-align'

" Browse GitHub events (user dashboard, user/repo activity) in Vim.
" https://github.com/junegunn/vim-github-dashboard
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" UltiSnips is the ultimate solution for snippets in Vim.
" https://github.com/SirVer/ultisnips
" Plug 'SirVer/ultisnips'

" vim-snipmate default snippet
" Plug 'honza/vim-snippets'

" On-demand loading
" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Clojure Fireplace https://github.com/tpope/vim-fireplace
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Generates a list of compiler flags. Using a non-master branch
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
" Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin options
" Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Unmanaged plugin (manually installed and updated)
" Plug '~/my-prototype-plugin'

Plug 'mhartington/oceanic-next'

" Plug 'leafgarland/typescript-vim'

" Plug 'vim-syntastic/syntastic'

" JSX highlight
"Plug 'pangloss/vim-javascript'
"Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty'

" Editor config
Plug 'editorconfig/editorconfig-vim'

" Clojure (lisp) formatter
" Plug 'snoe/nvim-parinfer.js'
" Plug 'bhurlow/vim-parinfer'

" Formatter
Plug 'sbdchd/neoformat'

" Autocomplete
" Plug 'valloric/youcompleteme'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'clojure-vim/async-clj-omni'

" Initialize plugin system
call plug#end()

if (has("termguicolors"))
 set termguicolors
endif

" Theme
syntax enable
set background=dark
colorscheme OceanicNext

" Tab and indent width
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab

" Invisible characters (ttf-droid)
set listchars=tab:▸\ ,eol:¬,trail:·,nbsp:·,extends:>,precedes:<
set list

" Other
set number
set colorcolumn=80
"let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" Clojure
"
syntax on
filetype plugin indent on

let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

" Enagle autocomplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

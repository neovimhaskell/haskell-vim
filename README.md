Haskell Syntax/Indentation
==========================

I was unhappy with the Haskell script that are
shipped with vim, therefore I decided to make my
own based on idris-vim.

I hope you find this useful.

![Screenshot](http://raichoo.github.io/images/haskell-vim.png)

## Installation

I recommend using [Pathogen][] for installation. Simply clone
this repo into your `~/.vim/bundle` directory and you are ready to go.

    cd ~/.vim/bundle
    git clone https://github.com/raichoo/haskell-vim.git

### Manual Installation

Copy content into your `~/.vim` directory.

Be sure that the following lines are in your
`.vimrc`


    syntax on
    filetype on
    filetype plugin indent on

[Pathogen]: https://github.com/tpope/vim-pathogen

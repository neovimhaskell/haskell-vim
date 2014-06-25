Haskell Syntax/Indentation
==========================

I was unhappy with the Haskell scripts that are
shipped with vim, therefore I decided to make my
own based on [idris-vim][].

I hope you find this useful.

![Screenshot](http://raichoo.github.io/images/haskell-vim.png)

## Features

* Covers a broader spectrum of keywords
* Highlighting for new features like type families
* More contextual highlighting (e.g. highlight 'as' or 'family' only in approriate places)
* Smarter indentation

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
    
## Configuration

### Features

* `let g:haskell_enable_quantification = 1` to enable highlighting of `forall` and `exists`
* `let g:haskell_enable_recursivedo = 1` to enable highlighting of `mdo` and `rec`
* `let g:haskell_enable_arrowsyntax = 1` to enable highlighting of `proc`

### Indentation

To configure indentation in `haskell-vim` you can use the following variables:

* `let g:haskell_indent_if = 3`

        if bool
        >>>then ...
        >>>else ...
  
* `let g:haskell_indent_case = 5`

        case xs of
        >>>>>[]     -> ...
        >>>>>(y:ys) -> ...
    
* `let g:haskell_indent_let = 4`

        let x = 0 in
        >>>>x
  
* `let g:haskell_indent_where = 6`

        where f :: Int -> Int
        >>>>>>f x = x
  
* `let g:haskell_indent_do = 3`
        
        do x <- a
        >>>y <- b
  


[Pathogen]: https://github.com/tpope/vim-pathogen
[idris-vim]: https://github.com/idris-hackers/idris-vim

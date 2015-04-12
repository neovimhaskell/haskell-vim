Haskell Syntax/Indentation
==========================

I was unhappy with the Haskell scripts that are
shipped with vim, therefore I decided to make my
own based on [idris-vim][].

I hope you find this useful.

![Screenshot](http://raichoo.github.io/images/haskell-vim.png)

## Features

* Covers a broader spectrum of keywords
* Highlighting for new features like type families, pattern synonyms, arrow syntax, recursive do, role annotations, QuasiQuotation
* More contextual highlighting (e.g. highlight 'as' or 'family' only in appropriate places)
* Smarter indentation
* Better Cabal support (WIP)

## Installation

I recommend using [Pathogen][] for installation. Simply clone
this repo into your `~/.vim/bundle` directory and you are ready to go.

    cd ~/.vim/bundle
    git clone https://github.com/raichoo/haskell-vim.git

Be sure that the following lines are in your
`.vimrc`


    syntax on
    filetype on
    filetype plugin indent on

### Manual Installation

Copy content into your `~/.vim` directory.

## Configuration

### Features

To enable the features you would like to use, just add the according line to your
`.vimrc`.

* `let g:haskell_enable_quantification = 1` to enable highlighting of `forall`
* `let g:haskell_enable_recursivedo = 1` to enable highlighting of `mdo` and `rec`
* `let g:haskell_enable_arrowsyntax = 1` to enable highlighting of `proc`
* `let g:haskell_enable_pattern_synonyms = 1` to enable highlighting of `pattern`
* `let g:haskell_enable_typeroles = 1` to enable highlighting of type roles
* `let g:haskell_enable_static_pointers = 1` to enable highlighting of `static`

### Indentation

To configure indentation in `haskell-vim` you can use the following variables to change indentation depth, just add the according line to your `.vimrc`.

#### Haskell

* `let g:haskell_indent_if = 3`

        if bool
        >>>then ...
        >>>else ...

* `let g:haskell_indent_case = 2`

        case xs of
        >>[]     -> ...
        >>(y:ys) -> ...

* `let g:haskell_indent_let = 4`

        let x = 0 in
        >>>>x

* `let g:haskell_indent_where = 6`

        where f :: Int -> Int
        >>>>>>f x = x

* `let g:haskell_indent_do = 3`

        do x <- a
        >>>y <- b

* `let g:haskell_indent_in = 1`

        let x = 1
        >in x

#### Cabal

*  `let g:cabal_indent_section = 2` (limited to max. 4 spaces)

        executable name
        >>main-is:             Main.hs


[Pathogen]: https://github.com/tpope/vim-pathogen
[idris-vim]: https://github.com/idris-hackers/idris-vim

### Plugin Support

#### Haskell

`haskell-vim` comes with some helpful functionality for working with Haskell

* `HaskellAddModuleComment`: Adds a module comment block to the top of your haskell file

        {-|
        Module      : 
        Description : 
        Copyright   : 
        License     : 
        Maintainer  : 
        Stability   : 
        Portability : 
        -}

#### Cabal

`haskell-vim` comes with a few helpful commands for working with Cabal

* `CabalAddExecutable`: Adds a new executable section to the end of your cabal file. Prompts you for executable filename and source file.

        executable name
          -- ghc-options:
          main-is:             Main.hs
          -- other-modules:
          -- other-extensions:
          build-depends:       base
          -- hs-source-dirs:
          default-language:    Haskell2010

* `CabalAddLibrary`: Adds a new library section to the end of your cabal file. Prompts you for library name.

        library name
          -- ghc-options:
          -- other-modules:
          -- other-extensions:
          build-depends:       base
          -- hs-source-dirs:
          default-language:    Haskell2010

* `CabalAddFlag`: Adds a new flag section to the end of your cabal file. Prompts you for flag name.

        flag name
          description:
          default:      False
          manual:       True

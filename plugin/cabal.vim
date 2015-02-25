if exists("g:loaded_haskellvim_cabal")
  finish
endif

let g:loaded_haskellvim_cabal = 1

let s:exetmpl = [ "\nexecutable",
                \ "  ghc-options:",
                \ "  main-is:",
                \ "  -- other-modules:",
                \ "  -- other-extensions:",
                \ "  build-depends:       base",
                \ "  -- hs-source-dirs:",
                \ "  default-language:    Haskell2010"
                \ ]

let s:libtmpl = [ "\nlibrary",
                \ "  ghc-options:",
                \ "  -- other-modules:",
                \ "  -- other-extensions:",
                \ "  build-depends:       base",
                \ "  -- hs-source-dirs:",
                \ "  default-language:    Haskell2010"
                \ ]

let s:flagtmpl = [ "\nflag",
                 \ "  description:",
                 \ "  default:      False",
                 \ "  manual:       True",
                 \ ]

function! cabal#addExecutable()
  exe "normal Go" . join(s:exetmpl, "\n")
endfunction

function! cabal#addLibrary()
  exe "normal Go" . join(s:libtmpl, "\n")
endfunction

function! cabal#addFlag()
  exe "normal Go" . join(s:flagtmpl, "\n")
endfunction

command! CabalAddExecutable call cabal#addExecutable()
command! CabalAddLibrary call cabal#addLibrary()
command! CabalAddFlag call cabal#addFlag()

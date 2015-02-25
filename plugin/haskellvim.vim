if exists("g:loaded_haskellvim_cabal")
  finish
endif

let g:loaded_haskellvim_cabal = 1

function! s:exeTmpl(name, src)
  let l:exetmpl = [ "\nexecutable " . a:name,
                  \ "  -- ghc-options:",
                  \ "  main-is:             " . a:src,
                  \ "  -- other-modules:",
                  \ "  -- other-extensions:",
                  \ "  build-depends:       base",
                  \ "  -- hs-source-dirs:",
                  \ "  default-language:    Haskell2010"
                  \ ]

  return join(l:exetmpl, "\n")
endfunction

function! s:libTmpl(name)
  let l:libtmpl = [ "\nlibrary " . a:name,
                  \ "  -- ghc-options:",
                  \ "  -- other-modules:",
                  \ "  -- other-extensions:",
                  \ "  build-depends:       base",
                  \ "  -- hs-source-dirs:",
                  \ "  default-language:    Haskell2010"
                  \ ]

  return join(l:libtmpl, "\n")
endfunction

function! s:flagTmpl(name)
  let l:flagtmpl = [ "\nflag " . a:name,
                   \ "  description:",
                   \ "  default:      False",
                   \ "  manual:       True",
                   \ ]

  return join(l:flagtmpl, "\n")
endfunction

function! haskellvim#addExecutable(name, src)
  exe "normal Go" . s:exeTmpl(a:name, a:src)
endfunction

function! haskellvim#addLibrary(name)
  exe "normal Go" . s:libTmpl(a:name)
endfunction

function! haskellvim#addFlag(name)
  exe "normal Go" . s:flagTmpl(a:name)
endfunction

command! -nargs=* CabalAddExecutable call haskellvim#addExecutable(<f-args>)
command! -nargs=1 CabalAddLibrary call haskellvim#addLibrary(<f-args>)
command! -nargs=1 CabalAddFlag call haskellvim#addFlag(<f-args>)

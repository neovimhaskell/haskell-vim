if exists("g:loaded_haskellvim_cabal")
  finish
endif

let g:loaded_haskellvim_cabal = 1

function! s:makeSection(content)
  return "\n" . join(a:content, "\n")
endfunction

function! s:exeTmpl(name, src)
  let l:exetmpl = [ 'executable ' . a:name,
                  \ '-- ghc-options:',
                  \ 'main-is:             ' . a:src,
                  \ '-- other-modules:',
                  \ '-- other-extensions:',
                  \ 'build-depends:       base',
                  \ '-- hs-source-dirs:',
                  \ 'default-language:    Haskell2010'
                  \ ]

  return s:makeSection(l:exetmpl)
endfunction

function! s:libTmpl(name)
  let l:libtmpl = [ 'library ' . a:name,
                  \ '-- ghc-options:',
                  \ '-- other-modules:',
                  \ '-- other-extensions:',
                  \ 'build-depends:       base',
                  \ '-- hs-source-dirs:',
                  \ 'default-language:    Haskell2010'
                  \ ]

  return s:makeSection(l:libtmpl)
endfunction

function! s:flagTmpl(name)
  let l:flagtmpl = [ 'flag ' . a:name,
                   \ 'description:',
                   \ 'default:      False',
                   \ 'manual:       True',
                   \ ]

  return s:makeSection(l:flagtmpl)
endfunction

function! cabal#addExecutable(name, src)
  exe "normal Go" . s:exeTmpl(a:name, a:src)
endfunction

function! cabal#addLibrary(name)
  exe "normal Go" . s:libTmpl(a:name)
endfunction

function! cabal#addFlag(name)
  exe "normal Go" . s:flagTmpl(a:name)
endfunction

command! -buffer -nargs=* CabalAddExecutable call cabal#addExecutable(<f-args>)
command! -buffer -nargs=1 CabalAddLibrary call cabal#addLibrary(<f-args>)
command! -buffer -nargs=1 CabalAddFlag call cabal#addFlag(<f-args>)

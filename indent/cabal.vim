if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

setlocal indentexpr=GetCabalIndent()
setlocal indentkeys=!^F,o,O,<CR>

if !exists('g:cabal_indent_section')
  "executable name
  ">>main-is:             Main.hs
  let g:cabal_indent_section = 2
endif

function! GetCabalIndent()
  let l:prevline = getline(v:lnum - 1)

  if l:prevline =~ '^\([eE]xecutable\|[lL]ibrary\|[fF]lag\|[sS]ource-repository\)'
    return g:cabal_indent_section
  else
    return match(l:prevline, '\S')
  endif
endfunction

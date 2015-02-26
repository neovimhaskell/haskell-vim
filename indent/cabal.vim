" indentation for cabal
"
" author: raichoo (raichoo@googlemail.com)
"
if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

setlocal indentexpr=GetCabalIndent()
setlocal indentkeys=!^F,o,O,<CR>

function! GetCabalIndent()
  let l:prevline = getline(v:lnum - 1)

  if l:prevline =~ '^\([eE]xecutable\|[lL]ibrary\|[fF]lag\|[sS]ource-repository\)'
    return 2
  else
    return match(l:prevline, '\S')
  endif
endfunction

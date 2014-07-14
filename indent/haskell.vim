" indentation for haskell
"
" Based on idris indentation
"
" author: raichoo (raichoo@googlemail.com)
"
" Modify g:haskell_indent_if and g:haskell_indent_case to
" change indentation for `if'(default 3) and `case'(default 5).
" Example (in .vimrc):
" > let g:haskell_indent_if = 2

if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

if !exists('g:haskell_indent_if')
  " if bool
  " >>>then ...
  " >>>else ...
  let g:haskell_indent_if = 3
endif

if !exists('g:haskell_indent_case')
  " case xs of
  " >>>>>[]     -> ...
  " >>>>>(y:ys) -> ...
  let g:haskell_indent_case = 5
endif

if !exists('g:haskell_indent_let')
  " let x = 0 in
  " >>>>x
  let g:haskell_indent_let = 4
endif

if !exists('g:haskell_indent_where')
  " where f :: Int -> Int
  " >>>>>>f x = x
  let g:haskell_indent_where = 6
endif

if !exists('g:haskell_indent_do')
  " do x <- a
  " >>>y <- b
  let g:haskell_indent_do = 3
endif

setlocal indentexpr=GethaskellIndent()
setlocal indentkeys=!^F,o,O,\|,0=where,0=in,0=let,0=deriving,<CR>

function! GethaskellIndent()
  let l:prevline = getline(v:lnum - 1)
  let l:line = getline(v:lnum)

  if l:line =~ '\C^\s*\<where\>'
    let l:s = match(l:prevline, '\S')
    return l:s + 2
  endif

  if l:line =~ '\C^\s*\<deriving\>'
    let l:s = match(l:prevline, '\C\<\(newtype\|data\)\>')
    if l:s >= 0
      return l:s + 2
    endif
  endif

  if l:line =~ '\C^\s*\<let\>'
    let l:s = match(l:prevline, '\C\<let\>')
    if l:s != 0
      return l:s
    endif
  endif

  if l:line =~ '\C^\s\+\<in\>'
    let l:n = v:lnum
    let l:s = 0
    let l:stop = 0

    while l:s <= 0 && l:n > 0 && l:stop == 0
      let l:n = l:n - 1
      let l:l = getline(l:n)

      if match(l:l, '\C\<in\>') > 0 || match(l:l, '\S') == 0
        let l:stop = 1
      else
        let l:s = match(l:l, '\C\<let\>')
      endif
    endwhile

    if l:stop == 0 && l:n > 0
      return l:s + 1
    endif
  endif

  if l:prevline =~ '[!#$%&*+./<>?@\\^|~-]\s*$'
    let l:s = match(l:prevline, '=')
    if l:s > 0
      return l:s + 2
    endif

    let l:s = match(l:prevline, ':')
    if l:s > 0
      return l:s + 3
    else
      return match(l:prevline, '\S')
    endif
  endif

  if l:prevline =~ '[{([][^})\]]\+$'
    return match(l:prevline, '[{([]')
  endif

  if l:prevline =~ '\C\<let\>\s\+.\+\(\<in\>\)\?\s*$'
    return match(l:prevline, '\C\<let\>') + g:haskell_indent_let
  endif

  if l:prevline !~ '\C\<else\>'
    let l:s = match(l:prevline, '\C\<if\>.*\&.*\zs\<then\>')
    if l:s > 0
      return l:s
    endif

    let l:s = match(l:prevline, '\C\<if\>')
    if l:s > 0
      return l:s + g:haskell_indent_if
    endif
  endif

  if l:prevline =~ '\C\(\<where\>\|\<do\>\|=\|[{([]\)\s*$'
    return match(l:prevline, '\S') + &shiftwidth
  endif

  if l:prevline =~ '\C\<where\>\s\+\S\+.*$'
    return match(l:prevline, '\C\<where\>') + g:haskell_indent_where
  endif

  if l:prevline =~ '\C\<do\>\s\+\S\+.*$'
    return match(l:prevline, '\C\<do\>') + g:haskell_indent_do
  endif

  if l:prevline =~ '\C^\s*\<data\>\s\+[^=]\+\s\+=\s\+\S\+.*$'
    if l:line =~ '^\s*|'
      return match(l:prevline, '=')
    endif
  endif

  if l:line =~ '^\s*|'
    let l:n = v:lnum - 1
    let l:s = 0

    while l:s <= 0 && l:n > 0
      let l:s = match(getline(l:n), '\S')
      let l:n = l:n - 1
    endwhile

    return l:s
  endif

  if l:prevline =~ '\C\<case\>\s\+.\+\<of\>\s*$'
    return match(l:prevline, '\C\<case\>') + g:haskell_indent_case
  endif

  if l:prevline =~ '\C^\s*\<\data\>\s\+\S\+\s*$'
    return match(l:prevline, '\C\<data\>') + &shiftwidth
  endif

  return match(l:prevline, '\S')
endfunction

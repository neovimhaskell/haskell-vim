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
setlocal indentkeys=!^F,o,O,},0=where,0=in,0=let,<CR>

function! GethaskellIndent()
  let prevline = getline(v:lnum - 1)
  let line = getline(v:lnum)

  if line =~ '^\s*\<where\>'
    let s = match(prevline, '\S')
    return s + 2
  endif

  if line =~ '^\s*\<let\>'
    let s = match(prevline, '\<let\>')
    if s != 0
      return s
    endif
  endif

  if line =~ '^\s\+\<in\>'
    let n = v:lnum
    let s = 0

    while s <= 0 && n > 0
      let n = n - 1
      let l = getline(n)

      if l =~ '^\S'
        return 0
      endif

      let s = match(l,'\<let\>')
    endwhile

    return s + 1
  endif

  if prevline =~ '[!#$%&*+./<>?@\\^|~-]\s*$'
    let s = match(prevline, '=')
    if s > 0
      return s + 2
    endif

    let s = match(prevline, ':')
    if s > 0
      return s + 3
    else
      return match(prevline, '\S')
    endif
  endif

  if prevline =~ '[{([][^})\]]\+$'
    return match(prevline, '[{([]')
  endif

  if prevline =~ '\<let\>\s\+.\+\(\<in\>\)\?\s*$'
    return match(prevline, '\<let\>') + g:haskell_indent_let
  endif

  if prevline !~ '\<else\>'
    let s = match(prevline, '\<if\>.*\&.*\zs\<then\>')
    if s > 0
      return s
    endif

    let s = match(prevline, '\<if\>')
    if s > 0
      return s + g:haskell_indent_if
    endif
  endif

  if prevline =~ '\(\<where\>\|\<do\>\|=\|[{([]\)\s*$'
    return match(prevline, '\S') + &shiftwidth
  endif

  if prevline =~ '\<where\>\s\+\S\+.*$'
    return match(prevline, '\<where\>') + g:haskell_indent_where
  endif

  if prevline =~ '\<do\>\s\+\S\+.*$'
    return match(prevline, '\<do\>') + g:haskell_indent_do
  endif

  if prevline =~ '^\s*\<data\>\s\+[^=]\+\s\+=\s\+\S\+.*$'
    return match(prevline, '=')
  endif

  if prevline =~ '\<case\>\s\+.\+\<of\>\s*$'
    return match(prevline, '\<case\>') + g:haskell_indent_case
  endif

  if prevline =~ '^\s*\<\data\>\s\+\S\+\s*$'
    return match(prevline, '\<data\>') + &shiftwidth
  endif

  if (line =~ '^\s*}\s*' && prevline !~ '^\s*;')
    return match(prevline, '\S') - &shiftwidth
  endif

  return match(prevline, '\S')
endfunction

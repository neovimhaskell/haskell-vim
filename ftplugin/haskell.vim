if exists("g:loaded_haskellvim_haskell")
  finish
endif

let g:loaded_haskellvim_haskell = 1

function! haskell#makeModuleCommentBlock()
  let l:commenttmpl = [ '{-|',
                      \ 'Module      : ',
                      \ 'Description : ',
                      \ 'Copyright   : ',
                      \ 'License     : ',
                      \ 'Maintainer  : ',
                      \ 'Stability   : ',
                      \ 'Portability : ',
                      \ '-}']

  exe "normal ggO" . join(l:commenttmpl, "\n")
endfunction

command! -buffer -nargs=0 HaskellAddModuleComment call haskell#makeModuleCommentBlock()

if (has('nvim'))
  if !exists('g:ghc_mod_executable')
    let g:ghc_mod_executable = "ghc-mod"
  endif

  let s:stdout_buffer = []

  function! haskell#caseSplitHandler(job, data, event)
    if (a:event == "stdout")
      let s:stdout_buffer = s:stdout_buffer + a:data
    elseif (a:event == "exit")
      let l:res = join(s:stdout_buffer, "\n")
      let l:exp = '^\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) "\(.*\)"'
      let l:mth = matchlist(l:res, l:exp)

      if (len(l:mth) >= 6)
        let l:mrk = getpos("'a")

        call cursor(l:mth[3], l:mth[4])
        normal ma
        call cursor(l:mth[1], l:mth[2])
        normal d`ax
        exe "normal i" . l:mth[5]
        call cursor(l:mth[1], l:mth[2])

        call setpos("'a", l:mrk)
      endif

      let s:stdout_buffer = []
    endif
  endfunction

  function! haskell#caseSplit()
    update
    let l:cmd    = [ g:ghc_mod_executable,
                   \ "-b",
                   \ '\n',
                   \ "split",
                   \ expand('%'),
                   \ line("."),
                   \ virtcol(".") ]

    let l:handlers = { 'on_stdout': function('haskell#caseSplitHandler'),
                     \ 'on_exit': function('haskell#caseSplitHandler') }

    let l:ghcmod = jobstart(l:cmd, l:handlers)
  endfunction

  function! haskell#addDeclHandler(job, data, event)
    if (a:event == "stdout")
      let s:stdout_buffer = s:stdout_buffer + a:data
    elseif (a:event == "exit")
      let l:res = join(s:stdout_buffer, "\n")
      let l:exp = '^function\n\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\)\n\(.*\)'
      let l:mth = matchlist(l:res, l:exp)

      if (len(l:mth) >= 6)
        call cursor(l:mth[3], l:mth[4])
        exe "normal o" . l:mth[5]
        call cursor(l:mth[1], l:mth[2])
      endif

      let s:stdout_buffer = []
    endif
  endfunction

  function! haskell#addDecl()
    update
    let l:cmd    = [ g:ghc_mod_executable,
                   \ "sig",
                   \ expand('%'),
                   \ line("."),
                   \ virtcol(".") ]

    let l:handlers = { 'on_stdout': function('haskell#addDeclHandler'),
                     \ 'on_exit': function('haskell#addDeclHandler') }

    let l:ghcmod = jobstart(l:cmd, l:handlers)
  endfunction

  command! -buffer -nargs=0 HaskellCaseSplit call haskell#caseSplit()
  command! -buffer -nargs=0 HaskellAddDecl call haskell#addDecl()
endif

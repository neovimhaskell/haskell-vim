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
  let s:stdout_buffer = []

  function! haskell#caseSplitHandler(job, data, event)
    if (a:event == "stdout")
      let s:stdout_buffer = s:stdout_buffer + a:data
    elseif (a:event == "exit")
      let l:mrk = getpos("'a")
      let l:res = join(s:stdout_buffer, "\n")
      let l:exp = '^\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) "\(.*\)"'
      let l:mth = matchlist(l:res, l:exp)

      call cursor(l:mth[3], l:mth[4])
      normal ma
      call cursor(l:mth[1], l:mth[2])
      normal d`ax

      exe "normal i" . l:mth[5]
      call cursor(l:mth[1], l:mth[2])

      let s:stdout_buffer = []
      call setpos("'a", l:mrk)
    endif
  endfunction

  function! haskell#caseSplit()
    update
    let l:cmd    = [ "ghc-mod",
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
  command! -buffer -nargs=0 HaskellCaseSplit call haskell#caseSplit()
endif

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

  let s:resp1 = '^\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) "\(.*\)"'
  let s:resp2 = '^function\n\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\)\n\(.*\)'

  let s:ghc_modi_buffer = []
  let s:command_queue   = []

  function! s:process_cmd(rsp, exp, cmd, action)
    let l:mth = matchlist(a:rsp, a:exp)
    if (len(l:mth) >= 6)
      call a:action(a:cmd, l:mth)
    endif
  endfunction

  function! s:process_ghc_modi_buffer()
    let l:rsp = join(s:ghc_modi_buffer, "\n")
    let l:cmd = s:command_queue[0]

    let s:command_queue = s:command_queue[1:]

    if l:cmd == "sig"
      call s:process_cmd(l:rsp, s:resp2, "normal o", function('s:insertBlock'))
    elseif l:cmd == "refine"
      call s:process_cmd(l:rsp, s:resp1, "normal a", function('s:replaceBlock'))
    elseif l:cmd == "split"
      call s:process_cmd(l:rsp, s:resp1, "normal i", function('s:replaceBlock'))
    endif
  endfunction

  function! s:ghc_modi_handler(job, data, event)
    let l:skipnext = 0
    for i in a:data
      if !l:skipnext
        if (i == "OK")
          call s:process_ghc_modi_buffer()
          let s:ghc_modi_buffer = []
          let l:skipnext = 1
        else
          let s:ghc_modi_buffer += [i]
        endif
      else
        let l:skipnext = 0
      endif
    endfor
  endfunction

  let s:ghc_modi_job = jobstart( ["ghc-modi"],
                                 \ { 'on_stdout': function('s:ghc_modi_handler'),
                                 \   'on_exit': function('s:ghc_modi_handler') }
                              \)

  function! s:runCmd(cmd)
    update
    let s:command_queue += [a:cmd[0]]
    call jobsend(s:ghc_modi_job, join(a:cmd, " ") . "\n")
  endfunction

  function! s:replaceBlock(cmd, mth)
    let l:mrk = getpos("'a")

    call cursor(a:mth[3], a:mth[4])
    normal ma
    call cursor(a:mth[1], a:mth[2])
    normal d`ax
    exe a:cmd . a:mth[5]
    call cursor(a:mth[1], a:mth[2])

    call setpos("'a", l:mrk)
  endfunction

  function! s:insertBlock(cmd, mth)
    call cursor(a:mth[3], a:mth[4])
    exe a:cmd . a:mth[5]
    call cursor(a:mth[1], a:mth[2])
  endfunction

  function! haskell#caseSplit()
    let l:cmd = [ "split",
                \ expand('%'),
                \ line("."),
                \ virtcol(".") ]

    call s:runCmd(l:cmd)
  endfunction

  function! haskell#addDecl()
    let l:cmd = [ "sig",
                \ expand('%'),
                \ line("."),
                \ virtcol(".") ]

    call s:runCmd(l:cmd)
  endfunction

  function! haskell#refine()
    let l:expr = input("Enter expression: ")
    let l:cmd  = [ "refine",
                 \ expand('%'),
                 \ line("."),
                 \ virtcol("."),
                 \ l:expr ]

    call s:runCmd(l:cmd)
  endfunction

  command! -buffer -nargs=0 HaskellCaseSplit call haskell#caseSplit()
  command! -buffer -nargs=0 HaskellAddDecl call haskell#addDecl()
  command! -buffer -nargs=0 HaskellRefine call haskell#refine()
endif

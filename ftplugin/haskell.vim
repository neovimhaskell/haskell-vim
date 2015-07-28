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

  function! s:runJob(cmd, handler)
    update
    let l:handlers = { 'on_stdout': function(a:handler),
                     \ 'on_exit': function(a:handler) }

    let l:ghcmod = jobstart(a:cmd, l:handlers)

    call jobwait([l:ghcmod])
  endfunction

  function! s:replaceBlock(cmd, mth)
    call cursor(a:mth[3], a:mth[4])
    normal ma
    call cursor(a:mth[1], a:mth[2])
    normal d`ax
    exe a:cmd . a:mth[5]
    call cursor(a:mth[1], a:mth[2])
  endfunction

  function! s:insertBlock(cmd, mth)
    call cursor(a:mth[3], a:mth[4])
    exe a:cmd . a:mth[5]
    call cursor(a:mth[1], a:mth[2])
  endfunction

  function! s:mkJobHandler(regex, cmd, data, event, action)
    if (a:event == "stdout")
      let s:stdout_buffer = s:stdout_buffer + a:data
    elseif (a:event == "exit")
      let l:res = join(s:stdout_buffer, "\n")
      let l:mth = matchlist(l:res, a:regex)

      if (len(l:mth) >= 6)
        let l:mrk = getpos("'a")

        call a:action(a:cmd, l:mth)

        call setpos("'a", l:mrk)
      endif

      let s:stdout_buffer = []
    endif
  endfunction

  function! haskell#caseSplitHandler(job, data, event)
    let l:exp = '^\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) "\(.*\)"'
    let l:cmd = "normal i"

    call s:mkJobHandler(l:exp, l:cmd, a:data, a:event, function('s:replaceBlock'))
  endfunction

  function! haskell#caseSplit()
    let l:hdl = 'haskell#caseSplitHandler'
    let l:cmd = [ g:ghc_mod_executable,
                \ "-b",
                \ '\n',
                \ "split",
                \ expand('%'),
                \ line("."),
                \ virtcol(".") ]

    call s:runJob(l:cmd, l:hdl)
  endfunction

  function! haskell#addDeclHandler(job, data, event)
    let l:exp = '^function\n\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\)\n\(.*\)'
    let l:cmd = "normal o"

    call s:mkJobHandler(l:exp, l:cmd, a:data, a:event, function('s:insertBlock'))
  endfunction

  function! haskell#addDecl()
    let l:hdl = 'haskell#addDeclHandler'
    let l:cmd = [ g:ghc_mod_executable,
                \ "sig",
                \ expand('%'),
                \ line("."),
                \ virtcol(".") ]

    call s:runJob(l:cmd, l:hdl)
  endfunction

  function! haskell#refineHandler(job, data,event)
    let l:exp = '^\([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) "\(.*\)"'
    let l:cmd = "normal a"

    call s:mkJobHandler(l:exp, l:cmd, a:data, a:event, function('s:replaceBlock'))
  endfunction

  function! haskell#refine()
    let l:expr = input("Enter expression: ")
    let l:hdl  = 'haskell#refineHandler'
    let l:cmd  = [ g:ghc_mod_executable,
                 \ "refine",
                 \ expand('%'),
                 \ line("."),
                 \ virtcol("."),
                 \ l:expr ]

    call s:runJob(l:cmd, l:hdl)
  endfunction

  command! -buffer -nargs=0 HaskellCaseSplit call haskell#caseSplit()
  command! -buffer -nargs=0 HaskellAddDecl call haskell#addDecl()
  command! -buffer -nargs=0 HaskellRefine call haskell#refine()
endif

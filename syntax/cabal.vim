" syntax highlighting for cabal
"
" author: raichoo (raichoo@googlemail.com)

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn match cabalLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell
syn match cabalIdentifier "[A-Za-z\-]*" contained
syn match cabalOperator "[<=>&]"
syn match cabalColon ":" contained
syn match cabalNumber "\<[0-9][0-9\.]*\>"
syn match cabalDelimiter ","
syn match cabalSection "^\([eE]xecutable\|[lL]ibrary\|[fF]lag\|[sS]ource-repository\)"
syn match cabalEntry "^\s*[A-Za-z][a-zA-Z\-]*:" contains=cabalIdentifier,cabalColon
syn keyword cabalBool True False
syn keyword cabalConditional if
syn match cabalCompilerFlag "\s\+-[^ -][^ ]*"

syn region cabalDescription start="[dD]escription:" end="^\([^ ]\|$\)" contains=cabalEntry keepend

highlight def link cabalIdentifier Identifier
highlight def link cabalLineComment Comment
highlight def link cabalOperator Operator
highlight def link cabalColon Operator
highlight def link cabalNumber Number
highlight def link cabalSection Structure
highlight def link cabalDelimiter Delimiter
highlight def link cabalBool Boolean
highlight def link cabalCompilerFlag Macro
highlight def link cabalConditional Conditional

let b:current_syntax = "cabal"

" syntax highlighting for haskell
"
" Heavily modified version of the haskell syntax
" highlighter to support haskell.
"
" author: raichoo (raichoo@googlemail.com)
" date: Jun 18 2014

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn match haskellModule "\<module\>"
syn match haskellImport "\(\<import\>\(\s\+safe\)\?\|\<hiding\>\)"
syn match haskellForeign "\<foreign\>\s\+\<\(export\|import\)\>\(\s\+\(\<ccall\>\(\s\+\<\(\(un\)\?safe\|interruptible\)\>\)\?\|\<capi\>\|\<prim\>\)\>\)\?"
syn region haskellQualifiedImport start="\<qualified\>" contains=haskellType,haskellDot end="\<as\>"
syn match haskellStructure "\<\(class\|instance\|where\|newtype\|deriving\)\>"
syn match haskellDatatypes "\<\(data\|type\)\>\(\s\+\<family\>\)\?"
syn match haskellStatement "\<\(do\|case\|of\|let\|in\)\>"
syn match haskellConditional "\<\(if\|then\|else\)\>"
syn match haskellNumber "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match haskellDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"
syn match haskellInfix "\<\(infix\|infixl\|infixr\)\>"
syn match haskellOperators "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match haskellDot "\."
syn match haskellQuantification "\<\(forall\|exists\)\>"
syn match haskellType "\<\([A-Z][a-zA-Z0-9_]*\|_|_\)\>"
syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
syn match haskellChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"
syn match haskellBacktick "`[A-Za-z][A-Za-z0-9_]*`"
syn region haskellString start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region haskellBlockComment start="{-" end="-}" contains=haskellBlockComment

highlight def link haskellImport Structure
highlight def link haskellForeign Structure
highlight def link haskellQualifiedImport Structure
highlight def link haskellModule Structure
highlight def link haskellStructure Structure
highlight def link haskellDatatypes Structure
highlight def link haskellStatement Statement
highlight def link haskellConditional Conditional
highlight def link haskellNumber Number
highlight def link haskellFloat Float
highlight def link haskellDelimiter Delimiter
highlight def link haskellInfix PreProc
highlight def link haskellOperators Operator
highlight def link haskellDot Operator
highlight def link haskellQuantification Operator
highlight def link haskellType Include
highlight def link haskellLineComment Comment
highlight def link haskellBlockComment Comment
highlight def link haskellString String
highlight def link haskellChar String
highlight def link haskellBacktick Operator

let b:current_syntax = "haskell"

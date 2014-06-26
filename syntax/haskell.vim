" syntax highlighting for haskell
"
" Heavily modified version of the haskell syntax
" highlighter to support haskell.
"
" author: raichoo (raichoo@googlemail.com)

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword haskellModule module
syn match haskellImport "\(\<import\>\(\s\+safe\)\?\|\<hiding\>\)"
syn match haskellForeign "\<foreign\>\s\+\<\(export\|import\)\>\(\s\+\(\<ccall\>\(\s\+\<\(\(un\)\?safe\|interruptible\)\>\)\?\|\<capi\>\|\<prim\>\)\>\)\?"
syn region haskellQualifiedImport start="\<qualified\>" contains=haskellType,haskellDot end="\<as\>"
syn keyword haskellStructure class instance where newtype deriving
syn match haskellDatatypes "\<\(data\|type\)\>\(\s\+\<family\>\)\?"
syn keyword haskellStatement do case of let in
syn keyword haskellConditional if then else
syn match haskellNumber "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match haskellDelimiter  "(\|)\|\[\|\]\|,\|;\|{\|}"
syn keyword haskellInfix infix infixl infixr
syn match haskellOperators "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match haskellDot "\."
syn match haskellType "\<[A-Z][a-zA-Z0-9_]*\>"
syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell
syn match haskellChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"
syn match haskellBacktick "`[A-Za-z][A-Za-z0-9_]*\('\)*`"
syn region haskellString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
syn region haskellBlockComment start="{-" end="-}" contains=haskellBlockComment,@Spell
syn match haskellIdentifier "[a-z][a-zA-z0-9_]*\('\)*" contained
syn match haskellTopLevelDecl "\s*[a-z][a-zA-z0-9_]*\('\)*\s*::" contains=haskellIdentifier,haskellOperators

if exists('g:haskell_enable_quantification')
  syn keyword haskellQuantifiers forall exists contained
  syn match haskellQuantification "\<\(forall\|exists\)\>\s\+[^.=]*\."
    \ contains=haskellQuantifiers,haskellOperators,haskellDot,haskellDelimiter
endif
if exists('g:haskell_enable_recursivedo')
  syn keyword haskellRecursiveDo mdo rec
endif
if exists('g:haskell_enable_arrowsyntax')
  syn keyword haskellArrowSyntax proc
endif

highlight def link haskellIdentifier Identifier
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
highlight def link haskellType Include
highlight def link haskellLineComment Comment
highlight def link haskellBlockComment Comment
highlight def link haskellString String
highlight def link haskellChar String
highlight def link haskellBacktick Operator

if exists('g:haskell_enable_quantification')
  highlight def link haskellQuantifiers Operator
endif
if exists('g:haskell_enable_recursivedo')
  highlight def link haskellRecursiveDo Operator
endif
if exists('g:haskell_enable_arrowsyntax')
  highlight def link haskellArrowSyntax Operator
endif

let b:current_syntax = "haskell"

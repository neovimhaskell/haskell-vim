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

syn keyword haskellBlockKeywords data type family module where class instance deriving contained
if exists('g:haskell_enable_pattern_synonyms')
  syn region haskellModuleBlock start="\<module\>" end="\<where\>"
    \ contains=haskellType,haskellDelimiter,haskellDot,haskellOperators,haskellModule,haskellBlockKeywords,haskellLineComment,haskellBlockComment,haskellPatternSynonyms keepend
else
  syn region haskellModuleBlock start="\<module\>" end="\<where\>"
    \ contains=haskellType,haskellDelimiter,haskellDot,haskellOperators,haskellModule,haskellBlockKeywords,haskellLineComment,haskellBlockComment keepend
endif
syn region haskellBlock start="\<\(class\|instance\)\>" end="\(\<where\>\|^\s*$\)"
  \ contains=haskellType,haskellDelimiter,haskellDot,haskellOperators,haskellModule,haskellBlockKeywords keepend
syn region haskellDataBlock start="\<\(data\|type\)\>\(\s\+\<family\>\)\?" end="\([=]\|\<where\>\|^\s*$\)" keepend
  \ contains=haskellType,haskellDelimiter,haskellDot,haskellOperators,haskellModule,haskellBlockKeywords keepend
syn keyword haskellStandaloneDerivingKeywords deriving instance contained
syn keyword haskellStructure newtype deriving default
syn region haskellStandaloneDeriving start="\<deriving\>\s\+\<instance\>" end="$"
  \ contains=haskellType,haskellDelimiter,haskellDot,haskellOperators,haskellStandaloneDerivingKeywords
syn keyword haskellImportKeywords import qualified safe as hiding contained
syn keyword haskellForeignKeywords foreign export import ccall safe unsafe interruptible capi prim contained
syn region haskellForeignImport start="\<foreign\>" contains=haskellString,haskellOperators,haskellForeignKeywords,haskellIdentifier end="::" keepend
syn region haskellImport start="\<import\>" contains=haskellDelimiter,haskellType,haskellDot,haskellImportKeywords end="\((\|$\)" keepend
syn keyword haskellStatement do case of let in where
syn keyword haskellConditional if then else
syn match haskellNumber "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match haskellDelimiter  "(\|)\|\[\|\]\|,\|;\|{\|}"
syn keyword haskellInfix infix infixl infixr
syn keyword haskellBottom undefined error
syn match haskellOperators "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match haskellDot "\."
syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell
syn match haskellBacktick "`[A-Za-z][A-Za-z0-9_\.]*'*`"
syn region haskellString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
syn region haskellBlockComment start="{-" end="-}" contains=haskellBlockComment,@Spell
syn match haskellIdentifier "[_a-z][a-zA-z0-9_]*'*" contained
syn match haskellChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"
syn match haskellType "\(\<[A-Z][a-zA-Z0-9_]*\>\|'\<[A-Z][a-zA-Z0-9_]*\>\)'*"
syn match haskellTopLevelDecl "^\s*\([_a-z][a-zA-z0-9_]*'*,\?\s*\)\+::" contains=haskellIdentifier,haskellOperators,haskellDelimiter
syn match haskellRecordField "[_a-z][a-zA-z0-9_]*'*\s*::" contains=haskellIdentifier,haskellOperators contained
syn region haskellRecordBlock start="[A-Z][a-zA-Z0-9]*'*\s\+{" end="}" keepend
  \ contains=haskellType,haskellDelimiter,haskellOperators,haskellDot,haskellRecordField,haskellString,haskellChar,haskellFloat,haskellNumber,haskellBacktick,haskellLineComment, haskellBlockComment,haskellBottom,haskellConditional,haskellStatement
syn region haskellRecordUpdate start="[a-z][a-zA-Z0-9]*'*\s\+{" end="}" keepend
  \ contains=haskellType,haskellDelimiter,haskellOperators,haskellDot,haskellString,haskellChar,haskellFloat,haskellNumber,haskellBacktick,haskellLineComment, haskellBlockComment,haskellBottom,haskellConditional,haskellStatement
syn region haskellQuasiQuote start="\[[_a-z][a-zA-z0-9_]*'*|" end="|\]" keepend

if exists('g:haskell_enable_typeroles')
  syn keyword haskellTypeRoles type role phantom representational nominal contained
  syn region haskellTypeRoleBlock start="type\s\+role" end="[\n]"
    \ contains=haskellType,haskellTypeRoles keepend
endif
if exists('g:haskell_enable_quantification')
  syn keyword haskellForall forall contained
  syn match haskellQuantification "\<\(forall\)\>\s\+[^.=]*\."
    \ contains=haskellForall,haskellOperators,haskellDot,haskellDelimiter
endif
if exists('g:haskell_enable_recursivedo')
  syn keyword haskellRecursiveDo mdo rec
endif
if exists('g:haskell_enable_arrowsyntax')
  syn keyword haskellArrowSyntax proc
endif
if exists('g:haskell_enable_pattern_synonyms')
  syn keyword haskellPatternSynonyms pattern
endif

highlight def link haskellBottom Macro
highlight def link haskellQuasiQuote String
highlight def link haskellBlockKeywords Structure
highlight def link haskellStandaloneDerivingKeywords Structure
highlight def link haskellIdentifier Identifier
highlight def link haskellImportKeywords Structure
highlight def link haskellForeignKeywords Structure
highlight def link haskellStructure Structure
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
  highlight def link haskellForall Operator
endif
if exists('g:haskell_enable_recursivedo')
  highlight def link haskellRecursiveDo Operator
endif
if exists('g:haskell_enable_arrowsyntax')
  highlight def link haskellArrowSyntax Operator
endif
if exists('g:haskell_enable_pattern_synonyms')
  highlight def link haskellPatternSynonyms Structure
endif
if exists('g:haskell_enable_typeroles')
  highlight def link haskellTypeRoles Structure
endif

let b:current_syntax = "haskell"

if vim.b.current_syntax then
    return
end

vim.cmd([[
" Keywords with nextgroup for name highlighting
syn keyword oxigenKeyword fun nextgroup=oxigenFuncName skipwhite
syn keyword oxigenKeyword introduce intro from nextgroup=oxigenModule skipwhite
syn keyword oxigenKeyword struct nextgroup=oxigenStructName skipwhite
syn keyword oxigenKeyword contains
syn keyword oxigenKeyword main
syn keyword oxigenKeyword each repeat if option unless choose pattern when
syn keyword oxigenKeyword guard fail give skip stop
syn keyword oxigenKeyword in not and or then as hide self

syn keyword oxigenBoolean True False
syn keyword oxigenNone None

" Function, module, and struct names (contained = only match after their keyword)
syn match oxigenFuncName /\w\+/ contained
syn match oxigenModule /\w\+/ contained
syn match oxigenStructName /\w\+/ contained

" Builtins
syn keyword oxigenBuiltin print println len push first last rest
syn keyword oxigenBuiltin type ord chr str int float range chars
syn keyword oxigenBuiltin byte uint set keys values insert remove has
syn keyword oxigenBuiltin tuple error is_value is_error

" Angle form directives: <log>, <log<tag>>, <fail>, <guard>, <guard<...>>
syn match oxigenDirective /<log\(<[^>]*>\)\?>/
syn match oxigenDirective /<fail>/
syn match oxigenDirective /<guard\(<[^>]*>\)\?>/
syn match oxigenDirective /<Value>/

" Error/type constructors: <Error>, <Error<tag>>, <type<...>>
syn match oxigenErrorType /<Error\(<[^>]*>\)\?>/
syn match oxigenErrorType /<type<[^>]*>>/

" Type annotations: <int>, <str>, etc.
syn match oxigenType /<\(int\|str\|float\|char\|bool\|array\|byte\|uint\|tuple\|map\|set\|generic\|None\)>/

" Variable declarations: name := or name <type> :=
syn match oxigenVariable /\<\w\+\>\ze\s*<[^>]*>\s*:=\?/
syn match oxigenVariable /\<\w\+\>\ze\s*:=/

" Strings
syn region oxigenString start=/"/ skip=/\\"/ end=/"/ contains=oxigenInterp
syn region oxigenChar start=/'/ skip=/\\'/ end=/'/
syn match oxigenInterp /{\([^}]*\)}/ contained

" Numbers
syn match oxigenNumber /\<\d\+\>/
syn match oxigenFloat /\<\d\+\.\d\+\>/

" Comments
syn match oxigenComment /\/\/.*/
syn region oxigenComment start=/\/\*/ end=/\*\//

" Directives: #[indent], #[main]
syn match oxigenPreProc /#\[.\{-}\]/

" Operators
syn match oxigenOperator /:=/
syn match oxigenOperator /==/
syn match oxigenOperator /!=/
syn match oxigenOperator /+\+/
syn match oxigenOperator /--/
syn match oxigenOperator /\->/
syn match oxigenOperator /||/
syn match oxigenOperator /<<\|>>/
syn match oxigenOperator /<=\|>=/

" Highlight groups
hi def link oxigenKeyword Keyword
hi def link oxigenBoolean Boolean
hi def link oxigenNone Constant
hi def link oxigenBuiltin Function
hi def link oxigenString String
hi def link oxigenChar Character
hi def link oxigenInterp Special
hi def link oxigenNumber Number
hi def link oxigenFloat Float
hi def link oxigenComment Comment
hi def link oxigenOperator Operator
hi def link oxigenType Type
hi def link oxigenDirective Special
hi def link oxigenErrorType Type
hi def link oxigenVariable Identifier
hi def link oxigenFuncName Function
hi def link oxigenModule Include
hi def link oxigenStructName Type
hi def link oxigenPreProc PreProc
]])

vim.b.current_syntax = "oxigen"

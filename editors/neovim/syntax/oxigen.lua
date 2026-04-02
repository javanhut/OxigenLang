if vim.b.current_syntax then
    return
end

-- Keywords
vim.cmd([[
syn keyword oxigenKeyword fun struct contains introduce intro from
syn keyword oxigenKeyword each repeat if option unless choose pattern when
syn keyword oxigenKeyword guard fail give skip stop
syn keyword oxigenKeyword in not and or then as hide self

syn keyword oxigenBoolean True False
syn keyword oxigenNone None

" Builtins
syn keyword oxigenBuiltin print println len push first last rest
syn keyword oxigenBuiltin type ord chr str int float range chars
syn keyword oxigenBuiltin byte uint set keys values insert remove has
syn keyword oxigenBuiltin tuple error is_value is_error

" Strings
syn region oxigenString start=/"/ skip=/\\"/ end=/"/ contains=oxigenInterp
syn region oxigenChar start=/'/ skip=/\\'/ end=/'/
syn match oxigenInterp /{\([^}]*\)}/ contained

" Numbers
syn match oxigenNumber /\<\d\+\>/
syn match oxigenFloat /\<\d\+\.\d\+\>/

" Comments (single-line with //)
syn match oxigenComment /\/\/.*/

" Operators
syn match oxigenOperator /:=/
syn match oxigenOperator /==/
syn match oxigenOperator /!=/
syn match oxigenOperator /+\+/
syn match oxigenOperator /--/
syn match oxigenOperator /\->/
syn match oxigenOperator /||/

" Type annotations
syn match oxigenType /<\(int\|str\|float\|char\|bool\|array\|byte\|uint\|tuple\|map\|set\|generic\)>/

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
]])

vim.b.current_syntax = "oxigen"

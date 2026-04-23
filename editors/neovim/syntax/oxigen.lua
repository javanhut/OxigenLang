if vim.b.current_syntax then
    return
end

local function oxigen_regex_escape(text)
    return vim.fn.escape(text, "\\.^$~[]")
end

local function oxigen_sorted_list(set)
    local items = {}
    for name in pairs(set) do
        items[#items + 1] = name
    end
    table.sort(items)
    return items
end

local function oxigen_alt_pattern(names, include_self)
    local escaped = vim.tbl_map(oxigen_regex_escape, names)
    if include_self then
        escaped[#escaped + 1] = "self"
    end
    if #escaped == 0 then
        return nil
    end
    return "\\<\\%(" .. table.concat(escaped, "\\|") .. "\\)\\>"
end

local function oxigen_collect_symbols(bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local typed_vars = {}
    local untyped_vars = {}
    local struct_fields = {}
    local in_struct = false
    local struct_depth = 0

    for _, raw_line in ipairs(lines) do
        local line = raw_line:gsub("//.*$", "")
        local trimmed = vim.trim(line)

        if not in_struct and trimmed:match("^struct%s+[%a_][%w_]*") and trimmed:find("{", 1, true) then
            in_struct = true
            struct_depth = select(2, line:gsub("{", "")) - select(2, line:gsub("}", ""))
        elseif in_struct then
            local field_name = trimmed:match("^hide%s+([%a_][%w_]*)%s+<[^>]+>")
                or trimmed:match("^([%a_][%w_]*)%s+<[^>]+>")
            if field_name then
                struct_fields[field_name] = true
            end

            struct_depth = struct_depth + select(2, line:gsub("{", "")) - select(2, line:gsub("}", ""))
            if struct_depth <= 0 then
                in_struct = false
                struct_depth = 0
            end
        else
            for lhs in line:gmatch("([%a_][%w_%,%s]*)%s*:=") do
                if not lhs:find("<", 1, true) then
                    for name in lhs:gmatch("([%a_][%w_]*)") do
                        untyped_vars[name] = true
                    end
                end
            end

            for name in line:gmatch("([%a_][%w_]*)%s+<[^>]+>") do
                if name ~= "struct" and name ~= "hide" then
                    typed_vars[name] = true
                end
            end
        end
    end

    typed_vars.self = nil
    untyped_vars.self = nil

    return {
        typed_vars = oxigen_sorted_list(typed_vars),
        untyped_vars = oxigen_sorted_list(untyped_vars),
        struct_fields = oxigen_sorted_list(struct_fields),
    }
end

local function oxigen_define_dynamic_syntax(bufnr)
    if not vim.api.nvim_buf_is_valid(bufnr) then
        return
    end

    local symbols = oxigen_collect_symbols(bufnr)
    local exclude = " containedin=ALLBUT,oxigenString,oxigenChar,oxigenComment,oxigenType,oxigenDirective,oxigenErrorType"

    vim.api.nvim_buf_call(bufnr, function()
        vim.cmd([[
syntax clear oxigenTypedVarUse
syntax clear oxigenUntypedVarUse
syntax clear oxigenTypedReceiver
syntax clear oxigenUntypedReceiver
syntax clear oxigenTypedDot
syntax clear oxigenUntypedDot
syntax clear oxigenTypedFieldAccess
syntax clear oxigenUntypedFieldAccess
        ]])

        local typed_pattern = oxigen_alt_pattern(symbols.typed_vars, true)
        local untyped_pattern = oxigen_alt_pattern(symbols.untyped_vars, false)

        if #symbols.typed_vars > 0 then
            vim.cmd("syntax match oxigenTypedVarUse /" .. typed_pattern .. "/" .. exclude)
            vim.cmd("syntax match oxigenTypedReceiver /" .. typed_pattern .. "\\ze\\.\\h\\w*/" .. exclude)
            vim.cmd("syntax match oxigenTypedDot /" .. typed_pattern .. "\\zs\\.\\ze\\h\\w*/" .. exclude)
            vim.cmd("syntax match oxigenTypedFieldAccess /" .. typed_pattern .. "\\.\\zs\\h\\w*/" .. exclude)
        else
            vim.cmd([[syntax match oxigenTypedReceiver /\<self\>\ze\.\h\w*/]] .. exclude)
            vim.cmd([[syntax match oxigenTypedDot /\<self\>\zs\.\ze\h\w*/]] .. exclude)
            vim.cmd([[syntax match oxigenTypedFieldAccess /\<self\>\.\zs\h\w*/]] .. exclude)
        end

        if #symbols.untyped_vars > 0 then
            vim.cmd("syntax match oxigenUntypedVarUse /" .. untyped_pattern .. "/" .. exclude)
            vim.cmd("syntax match oxigenUntypedReceiver /" .. untyped_pattern .. "\\ze\\.\\h\\w*/" .. exclude)
            vim.cmd("syntax match oxigenUntypedDot /" .. untyped_pattern .. "\\zs\\.\\ze\\h\\w*/" .. exclude)
            vim.cmd("syntax match oxigenUntypedFieldAccess /" .. untyped_pattern .. "\\.\\zs\\h\\w*/" .. exclude)
        end
    end)
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

" Method calls: os.exec(...), strings.upper(...)
syn match oxigenMethodCall /\<\w\+\.\zs\w\+\ze\s*(/
syn match oxigenModuleRef /\<\w\+\ze\.\w\+\s*(/

" Struct reference before `contains`: LinkedList contains { ... }
syn match oxigenStructRef /\<\w\+\>\ze\s\+contains\>/

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
" User-defined struct/enum type annotations: <Node>, <LinkedList>, etc.
syn match oxigenType /<[A-Z]\w*>/

" Struct bodies and field declarations
syn region oxigenStructBody start=/\<struct\>\_s\+\w\+\%(([^)]*)\)\?\_s*{/ end=/}/ contains=oxigenKeyword,oxigenFieldModifier,oxigenStructFieldDecl,oxigenType,oxigenComment,oxigenPreProc keepend
syn keyword oxigenFieldModifier hide contained
syn match oxigenStructFieldDecl /\<\w\+\>\ze\s\+<[^>]\+>/ contained

" Typed declarations and parameters: name <type>
syn match oxigenTypedVarDecl /\<\w\+\>\ze\s\+<[^>]\+>/

" Untyped walrus declarations: name :=
syn match oxigenUntypedVarDecl /\<\w\+\>\ze\s*:=/

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

" Executable script header
syn match oxigenShebang /^#!.*/

" Directives: #[indent], #[location=...]
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
hi def link oxigenChar Character
hi def link oxigenInterp Special
hi def link oxigenNumber Number
hi def link oxigenFloat Float
hi def link oxigenComment Comment
hi def link oxigenShebang PreProc
hi def link oxigenOperator Operator
hi def link oxigenType Type
hi def link oxigenDirective Special
hi def link oxigenErrorType Type
hi def link oxigenFuncName Function
hi def link oxigenModule Include
hi def link oxigenStructName Type
hi def link oxigenStructRef Type
hi def link oxigenMethodCall Function
hi def link oxigenModuleRef Include
hi def link oxigenPreProc PreProc

hi def oxigenUntypedVarDecl guifg=#e06c75 ctermfg=204 gui=bold cterm=bold
hi def oxigenUntypedVarUse guifg=#e06c75 ctermfg=204
hi def oxigenUntypedReceiver guifg=#e06c75 ctermfg=204 gui=bold cterm=bold
hi def oxigenUntypedDot guifg=#e06c75 ctermfg=204
hi def oxigenUntypedFieldAccess guifg=#f7768e ctermfg=210

hi def oxigenTypedVarDecl guifg=#98c379 ctermfg=114 gui=bold cterm=bold
hi def oxigenTypedVarUse guifg=#98c379 ctermfg=114
hi def oxigenTypedReceiver guifg=#98c379 ctermfg=114 gui=bold cterm=bold
hi def oxigenTypedDot guifg=#98c379 ctermfg=114
hi def oxigenTypedFieldAccess guifg=#56b6c2 ctermfg=73

hi def oxigenStructFieldDecl guifg=#56b6c2 ctermfg=73 gui=bold cterm=bold
hi def oxigenString guifg=#ffd75f ctermfg=221
hi def link oxigenFieldModifier Keyword
]])

oxigen_define_dynamic_syntax(vim.api.nvim_get_current_buf())

local oxigen_syntax_group = vim.api.nvim_create_augroup("oxigen_dynamic_syntax", { clear = false })
vim.api.nvim_create_autocmd({ "BufEnter", "TextChanged", "TextChangedI", "InsertLeave" }, {
    group = oxigen_syntax_group,
    buffer = vim.api.nvim_get_current_buf(),
    callback = function(args)
        if vim.bo[args.buf].filetype == "oxigen" and vim.b[args.buf].current_syntax == "oxigen" then
            oxigen_define_dynamic_syntax(args.buf)
        end
    end,
})

vim.b.current_syntax = "oxigen"

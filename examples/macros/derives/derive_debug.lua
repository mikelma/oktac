local function starts_with(str, start)
   return str:sub(1, #start) == start
end

local function build_ptr_memb_access(parent, parent_ty, child_ty, memb_id)
    return { MemberAccessExpr = {                                                                             
        access_types = { child_ty },
        members = { {
            MemberId = memb_id
        } },
        parent = {
            UnaryExpr = {
                expr_ty = {
                    Struct = parent_ty
                },
                op = "Deref",
                value = {
                    Identifyer = parent
                },
                var_ty = {
                    Ref = {
                        Struct = parent_ty
                    }
                }
            }
        },
        parent_ty = {
            Struct = parent_ty
        }
    }}
end

-- Check if the input node is a struct or not
if okta.macro.input[1].StructProto == nil then
    okta.compiler_error({
        cause = "Derive macro `debug` can only be applied to struct types",
        help = "Consider removing `debug` derive macro from the non struct type"
    })
    return {} -- return default value
end

local struct = okta.macro.input[1].StructProto

local struct_name = struct.name;
local struct_name_lc = string.lower(struct_name);

local fmt_str = string.format('%s {\n', struct_name)

local values = {}

-- iterate through all the members
for i = 1, (#struct.members) do
    local name = struct.members[i][1]
    local type = struct.members[i][2]

    local fmt = nil
    local member = build_ptr_memb_access(struct_name_lc, struct_name, type, i-1) 

    if starts_with(type, "Int") or starts_with(type, "UInt") or type == "Boolean" then
        fmt = "%d"
        values[i] = member 

    elseif starts_with(type, "Float") then
        fmt = "%f"
        values[i] = member 

    elseif type == "Str" then
        fmt = "%s"
        cstr_call = okta.quote('@cstr("")')[1]
        cstr_params = cstr_call.FunCall.params
        cstr_params[1] = member
        values[i] = cstr_call

    else
        fmt = "<unknown>"
    end

    if not (fmt == nil) then
        fmt_str = fmt_str..'  '..name..' = '..fmt..'\n'
    end
end

fmt_str = fmt_str..'}\n'

-- get the AST of the debug function
dbg_fn = okta.quote(string.format([[
pub fun %s_dbg(%s: &%s) {
    printf(@cstr(""));
}]], struct_name_lc, struct_name_lc, struct_name, fmt_str))

-- modify the parameters of the printf call
printf_params = dbg_fn[2].FuncDecl.stmts.Stmts[1].FunCall.params

cstr_call = printf_params[1].FunCall
cstr_call.params = okta.quote('"'..fmt_str..'"')

for i=1,(#values) do
    printf_params[i+1] = values[i]
end

return dbg_fn

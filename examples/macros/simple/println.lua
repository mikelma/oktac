-- This function is used to determine if a table `tab` contains the value `val` inside.
local function has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end
    return false
end

local int_types = {"i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64"};
local float_types = {"f32", "f64"};
local string_type = "str";

local fmt_str = "";  -- format string to pass to the `printf` function

for i = 1, (#okta.macro.input) do -- for each input node
    -- obtain the i-th node of the AST
    local node = okta.macro.input[i]
    -- get the type of the node
    local node_ty = okta.node_type(node)

    --get separator
    local pre  = (i==1 and "" or "\t")

    -- if the node is a integer type
    if has_value(int_types, node_ty) then
        fmt_str = fmt_str..pre.."%d"

    -- if the node is a float type
    elseif has_value(float_types, node_ty) then
        fmt_str = fmt_str..pre.."%f"

    -- if the node is a string type
    elseif node_ty == string_type then
        fmt_str = fmt_str..pre.."%s"

    -- the node cannot be formatted using basic formatting
    else
        -- return a compilation error from inside the macro
        okta.compiler_error({
            cause = "Cannot format type "..node_ty.." with the default formatter"
        })
    end
end

fmt_str = fmt_str .. "\n"

local print_call = okta.quote("printf(@cstr(\""..fmt_str.."\"));")

local vals_str = ""; -- string containing the values to print

-- append parameters to the `printf` call
for i = 1, (#okta.macro.input) do
    local node = okta.macro.input[i]

    -- if the node is of string type, it has to be converted to a C string
    if okta.node_type(node) == "str" then
        local cstr = okta.quote("@cstr(\"\");")[1]
        cstr.FunCall.params[1] = node
        cstr.FunCall.ret_ty = {Ref = "UInt8"}

        node = cstr
    end

    print_call[1].FunCall.params[i+1] = node
end

return print_call

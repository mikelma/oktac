input = okta.macro.input[1]
new_type = "UInt8"

result = input

-- In functions, derive macros are executed twice:
-- when the function's prototype is generated and
-- when the function's declaration (function body)
-- is created. 
-- In the first pass, prototypes of new functions, 
-- structs or enums can be created, while in the second
-- pass the actual declarations of the new functions is 
-- generated.

-- first of all, let's determine if the macro is being 
-- executed in the prototype pass or in the declaration 
-- pass.
if input.FuncProto ~= nil then
    proto = result.FuncProto

    -- change the function's name
    proto.name = proto.name.."_"..string.lower(new_type)
    -- change the function's return type
    proto.ret_type = new_type
    
    -- change the type of all the input parameters to `new_type`
    for index, elem in pairs(proto.params) do
        elem[2] = new_type
    end
else
    func = result.FuncDecl 

    -- now we'll do the same things as in the `FuncProto` case
    -- NOTE: This could be done much cleaner, but the code is 
    -- repeated for clarity, as this code is just an example
    func.name = func.name.."_"..string.lower(new_type)
    func.ret_type = new_type

    for index, elem in pairs(func.params) do
        elem[2] = new_type
    end
end

return { result }

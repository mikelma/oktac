-- all generated AST nodes will be appended here
result = {} 

-- a copy of the input struct but with a different name 
pizza_struct = okta.macro.input[1]
pizza_struct.StructProto.name = "Pizza"

-- add the newly created struct to the result
result[1] = pizza_struct

-- we have to register the struct `Pizza` in order to use it inside
-- the `quote` function in the lines below
okta.register(pizza_struct)

-- the AST of a function is a list of two AST nodes, the function's
-- prototype (similar to a header) and the function declaration (includes
-- function's body)
new_func = okta.quote([[
pub fun pizza_print(pizza: &Pizza) {
    printf(@cstr("Pizza { x: %d, y: %f }\n"), *pizza.x, *pizza.y);
}]])

result[2] = new_func[1] -- append prototype
result[3] = new_func[2] -- append declaration

return result

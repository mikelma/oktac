local inspect = require('inspect')

result = {} 

-- a copy of the input struct but with a different name 
pizza_struct = okta.macro.input[1]
pizza_struct.StructProto.name = "Pizza"

result[1] = pizza_struct

okta.register(pizza_struct)

new_func = okta.quote([[
pub fun print_pizza(pizza: &Pizza) {
    printf(@cstr("Pizza { x: %d, y: %f }\n"), *pizza.x, *pizza.y);
}]])

result[2] = new_func[1]
result[3] = new_func[2]

return result

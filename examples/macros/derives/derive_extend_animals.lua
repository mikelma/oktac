-- new variants to add to the input enum prototype
new_variants = { "Octopus", "Fish" }

result = okta.macro.input[1]
enum_variants = result.EnumProto.variants

-- change the name of the enum
result.EnumProto.name = "ExtAnimal"

-- insert new variants
for _, new_variant in pairs(new_variants) do
    table.insert(enum_variants, #enum_variants+1, { new_variant, {} })
end

return { result }

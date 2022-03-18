<center>
<h1>Okta language reference</h1>

<img src='https://git.sr.ht/~mikelma/oktac/blob/main/assets/okta-ottis.png' width='70'>

Version: v0.2.0

</center>

## Comments

Single line comments start with the `#` character and end when the 
line breaks, any character in between is accepted.
Currently, okta does not support multi-line comments.

```
#  This is a comment
## This is another comment
```

## Values

### Basic types

| Type        | C equivalent                                        | Description                     |
|-------------|-----------------------------------------------------|---------------------------------|
| `i8`        | `char`                                              | 8 bit signed integer            |
| `u8`        | (`unsigned`) `char`                                 | 8 bit unsigned integer          |
| `i16`       | `short int` or `int`(\*)                            | 16 bit signed integer           |
| `u16`       | `unsigned short int` or `int`(\*)                   | 16 bit unsigned integer         |
| `i32`       | `int` or `long int`                                 | 32 bit signed integer           |
| `u32`       | `unsigned int` or `unsigned long int` (\*)          | 32 bit unsigned integer         |
| `i64`       | `long int` or `long long int`(\*)                   | 64 bit signed integer           |
| `u64`       | `unsigned long int` or `unsigned long long int`(\*) | 64 bit signed integer           |
| `f32`       | `float`                                             | 32 bit float                    |
| `f64`       | `double`                                            | 64 bit float                    |
| `bool`      | `bool`                                              | Boolean                         |
| `c_voidptr` | `*void`                                             | Mainly used to interface with C |

(\*) The size of some types in C is data model dependent.

The default type for integers is `i64`, and the default floating-point type is `f64`.
For example,

```
# here the type of `a`  cannot be inferred, thus, the type of `a` defaults to `i64` 
let a = 10;

# this behavior can be avoided by manually defining the type of the variable
let b: u8 = 10;

# in this case, the type of `c` can be inferred from the right-hand side expression
let c = b + 7;

# the same happens with floats:
let x = 4.2;      # the type of `x` defaults to `f64`
let y: f32 = 4.2; # override the default type by manually defining the type
let z = y - 1.0;  # here the type of `z` can be inferred: `f32`
```

### Structs

Structs in okta are very similar to `Rust` or `C` structs:

```
type Foo = struct {
    elem1: i32,
    elem2: f64,
    elem3: &Egg,
}

type Egg = struct {
    size: u8,
}
```

**NOTE:** Currently, okta structs are 
[packed](https://stackoverflow.com/questions/5473189/what-is-a-packed-structure-in-c)
by default. This is done to properly extract struct members from `enum` variants, as 
non-packed structs cause errors and a more complex procedure is needed to extract struct 
members from enum variants. Expect this to change soon, as packed structs 
are far from being efficient.

### Enums

Enum syntax and semantics are very similar to those in Rust enums.

```
# simple enum
type Color = enum {
    Brown,
    White,
    Black,
    Rainbow
}

# enum variants can contain some extra information
type Animal = enum {
    Fish,
    Octopus { is_ottis: bool },
    Dog { size: u8, color: Color },
}

fun main() {
    let fish = Animal:Fish;
    let octopus = Animal:Octopus { is_ottis = true };
    let pluto = Animal:Dog { size = 42, color = Color:Brown };
}
```

**Low-level representation of enums:**

Enums can have multiple variants, where, each variant can contain 
different extra information. Thus, enums are composed of two parts: 
the tag used to determine the variant of the enum, and an array of 
bytes to store the extra information an enum variant can contain. 

```
struct {
    tag: u8,
    bytes: [u8; MAX_SIZE]
}
```

Note that the length of the `bytes` array must be equal to 
the size of the largest enum variant (determined by the size of its members).

When an enum variant is created, first the tag corresponding to that variant is set,
then, if the variant contains some extra information, this information is stored as 
an array of bytes in the `bytes` field.

Then, to access the information inside an enum variant, the array of bytes 
has to be reinterpreted (bitcasted) as the type of information being accessed.

### Compound types

#### Array

Arrays are lists of elements of the same type, where the length of the list is known at compile time.
Unlike C, array indexing in okta is protected, thus, if an array is indexed out of bounds, a runtime error
is created and the program panics (exits with error status).

The array type is denoted as `[T; N]`, where `T` is the type of the elements and `n` the length of the array.

```
let a = [0, 1, 2, 3];

let b:[[i32; 2]; 2] = [[0, 1], [2, 3]];

let element = a[1]; # get second element of `a`
let element = a[4]; # runtime error: index out of bounds

let l = @len(a);
```

#### Slice

Slices are lists of elements of the same type, where the length of the list is not 
known at compile time. They are denoted as `[T]`, where `T` is the type of the elements.

For example, when an array is sliced, a slice is created, as the length of the slice cannot 
be known at compile time. Slices can be thought of as partial "views" of arrays. As indexing, 
slicing indices are checked in runtime for index out of bound errors.

```
let my_array = [0, 1, 2, 3, 4];

# the last index of the slice operation is not inclusive, the length of the slice is 4
let end = 4;
let my_slice = my_array[0..end]; 

# other examples
let a = my_array[..2] # this is the same as: my_array[0..2] 
let b = my_array[1..] # this is the same as: my_array[0..length_of_my_array] 
let c = my_array[..]  # this includes all the elements of my_array
```

Slices can also be indexed, or sliced again:

```
let a_number = my_slice[1];
let other_view = my_slice[..3];
```

**Low-level representation of slices**

The in-memory representation of slices is a struct of two elements, where the first element 
is the pointer to the first element of the slice, and the second is the length of the slice:

```
struct {
    first: &T
    length: u64,
}
```

where `T` is the type of the elements.

### Strings

Okta suppports UTF-8 strings:

```
let name: str = "Mikel";

let ottis = "There is an 🐙!";

let friends = "🐟, 🐋, 🐡 and 🐌"

let foo: str = "Осьминоги это круто"

let bar = "章魚很酷"
```

As lists and slices, strings can also be indexed, however, the 
result of a string indexation is a byte and the length of a string
is given by the number of bytes to represent the UTF-8 string 
(different to the number of characters).

For more information see the *Low-level representation of strings* section below.

**EScape sequences:**

|             | Name                                            |
|-------------|-------------------------------------------------|
| `\\`        | Backslash                                       |
| `\n`        | Newline                                         |
| `\t`        | Tab                                             |
| `\r`        | Carriage return                                 |
| `\u{7FFF}`  |	24-bit Unicode character code (up to 6 digits)  |

**Low-level representation of strings:**

In practice, `str` is just an alias for `[u8]` (unsigned byte slices).
When a string is defined in okta, the UTF-8 string is converted to its 
byte representation, and a null byte is appended as the final element of the string.
This is done to maintain compatibility with C strings.

For example, the string `"There is an 🐙!"` is converted to 
`"There is an \F0\9F\90\99!\00"` (note the `\00` byte in the end).

## Variables

Variables in okta are declared after the `let` keyword, followed by the name of 
the variable and its value:

```
let bar = 10;
```

When a normal, non-constant, variable is declared, it only lives in the scope it is declared in:

```
let ottis = "ottis";

if true {
    # variable `animal` only lives inside this scope
    let animal = "octopus";
    print(ottis, animal); # this is completly fine

} # variable `animal` goes out of scope

print(animal); # error here
``` 

In okta, all variables must be declared with an initial value:

```
let foo = 10; # this is ok
let foo;      # this is not ok
```

After its declaration, variables can mutate their value:

```
let foo = 10;
foo = 12;
```

Variables can also be shadowed:

```
let egg = 10;
if egg > 9 {
    let egg = 0; # shadowing
    exit(egg);   # exit value is `0`
}

exit(egg); # exit value is 10
```

### Constant variables

Constant variables are immutable variables that are visible from
everywhere inside the module they are declared in or imported to.

Their declaration can only happen in the unit's main scope, together with 
functions and type declarations. Constant declarations start with the `const`
keyword (can also start with `pub` for defining public visibility), followed 
by its identifier, its type (must always be provided), and the value (that 
must be known at compile-time).

```
# no `;` is required
const FOO: i32 = 42.0

pub const BAR: i32 = EGG + FOO * 2

const EGG: i32 = 8

pub const PIZZA: f64 = 1.111

fun main() {
    printf("foo is %d\n", FOO);

    FOO = 10; # this is not allowed
}
```

## Operators

**TODO**

## References

**TODO**

## Flow control

### `if` conditional

```
let num = 42;

# simple `if` statemet
if num > 40 {
    print("num is a big number");

} # note that `else` is not mandatory

# simple `if-else` statement
if num < 50 {
    print("num is lower than 50");

} else {
    print("num is a big big number")
}

# multiple if conditions
if num == 10 {
    print("You win!");

} elif num == 22 {
    print("cool number");

} elif num == 666 {
    print("satan");

} else { # `else` isn't mandatory
    print("fin");
}
```

### Loops


#### `loop`

Loop with no conditions.

Example:

```
loop {
    print("Non stop!");
}
```

#### `while`

The `while` loop repeatedly executes an expression until some 
condition is no longer true. 

```
# prints numbers from 1 to 42
let i=1;
while i < 43 {
    printf(@cstr("i=%d\n"), i);
    i = i + 1;
}
```

#### `for`

`for` loops in okta follow this syntax:

```
for <stmt-1>; <expr>; <stmt-2> {
    ... 
}
```

The `<stmt-1>` statement is optional, and corresponds to a statement that is 
executed before entering the `for` loop.
`<expr>` is checked before each iteration, must be evaluated to a boolean 
type value. If its value is `true`, another loop is run, and if its value is 
`false`, the `for` loop ends.
The `<stmt-2>` statement is run after each iteration, and as `<stmt-1>` it is optional.

`<stmt-1>` and `<stmt-2>` statements must be one of the following:

* Variable declaration
* Function call 
* Assignment 

In this example, the following `for` loops are equivalent:

```
# print digits from 0 to 9
for let i=0; i < 10 ; i=i+1 {
    print(i);
}


let i=0;
for i < 10 ; i=i+1 {
    printf(@cstr("i: %d\n"), i);
}

let i=0; 
for i < 10 { # only condition expression
    printf(@cstr("i: %d\n"), i);
    i = i + 1;
}
```

#### `break` statement

**TODO**

## Functions

In okta, functions are introduced by the `fun` keyword.

```
fun main(): i8 {
   foo(10, 32);

   ret 0;
}

fun foo(a: i32, b: i32): i32 {
    dummy();
    ret a + b;
}

fun dummy() {
    # nothing here
}
```

Functions can be called from functions declared before them: 
although `main` is declared before `foo`, `main` can call `foo`.
The user does not have to think about the order in which functions are declared.

Recursion is also supported!

```
fun factorial(n: i32): i32 {
    if n == 1 {
        ret n;
    } else {
        # woha! so recursive...
        ret n*factorial(n-1);
    }
}
```

### External functions

External C functions can be easily used from okta source code.

**TODO**

## Visibility

Symbols in okta are private by default. This means that all types, macros, and functions 
within a module are, by default, only visible inside the module they are declared in.

To make a type, function, or macro visible in other modules, their declaration 
must be preceded by the `pub` keyword:

```
# `Batman` is only visible to other symbols declared within this module 
type Batman = struct {
    age: i32,
    super_power: SuperPower,
    bat: bool,
}

# `Robin` can be imported and used from other modules!
pub type Robin = struct {
    age: i32,
    super_power: SuperPower,
}
```

## Modules 

Each okta source file is considered a `module`. Modules are units that (should) 
contain functions, types, or macros grouped by some common functionality. 

Public symbols from one module can be imported into another module by using the `use` 
keyword followed by the path to the module to import.

In file `foo.ok`:

```
pub type Foo = struct { a: i8 }

pub type Egg = struct { size: u32 }

# this type is private, and cannot be imported from other modules 
type Bar = struct { b: f32 }
```

In file `animals/animal.ok`:
```
type Animal = enum {
    Octopus,
    Dog,
    Fish,
}

fun is_terrestial(animal: &Animal): bool {
    if let Animal:Dog = *animal {
        ret true;
    } else {
        ret false;
    }
}
```

In file `main.ok`:

```
# import all public symbols from `foo.ok`: 
#     * Foo (struct)
#     * Egg (struct)
use foo 

fun main() {
    let f = Foo { a = 2 };
    let dinner = Egg { size = 42 };
}
```

## Built-in functions

In okta, the compiler directly implements some functions that are available to the 
user, these functions are referred to as built-in. Built-in functions are easily recognizable, 
as all built-in functions start with the `@` character.

* `@sizeof(T): u64`
    
    Takes a type `T` and returns its size in bytes.

    **Example:**

    ```
    let s = @sizeof(&i16); # s = 8
    ```

* `@bitcast(val: U, T): T`
    
    Convert `val` to type `T` without changing any bits 
    (see [LLVM ref](https://llvm.org/docs/LangRef.html#bitcast-to-instruction)).

    **Example:**

    ```
    let my_val: u8 = 42;
    let ptr_to_u8 = &my_val;
    let ptr_to_i8 = @bitcast(ptr_to_u8, &i8);
    ```

* `@cstr(val: str): &u8`
    
    Converts an okta `str` to a C string representation (pointer to `u8`).

    **Example:**

    ```
    printf(@cstr("Helope!\n"));
    ```

* `@slice(ptr: &T, len: u64): [T]`
    
    Creates an okta slice where the base pointer is the first parameter to the 
    built-in function and the second parameter is the length of the slice.
    The type of the output slice is determined by the type of the pointer (first 
    parameter) points to.

* `@len(val: T): u64`
    
    Returns the length of the given value. Where the type `T` of the given 
    value is an array, slice, or string.

* `@inttoptr(val: T, U): U`
    
    Returns a pointer type U, that points to the address given by the integer value `val`
    (see [LLVM ref](https://llvm.org/docs/LangRef.html#inttoptr-to-instruction)).

    **Example:**

    ```
    let a: &i8 = @inttoptr(3, &i8);
    printf(@cstr("%p\n"), a); # prints: 0x3
    ```

* `@ptrtoint(ptr: T, U): U`
    
    Converts a pointer of type `T`, to the integer type specified by `U`.
    (see [LLVM ref](https://llvm.org/docs/LangRef.html#ptrtoint-to-instruction)).

    **Example:**

    ```
    let a: &i8 = @inttoptr(100, &i8);
    printf(@cstr("%p\n"), a); # prints: 0x64
    printf(@cstr("%d\n"), @ptrtoint(a, u64)); # prints: 100
    ```

## Compilation options

**TODO**

## Macros

Macros in okta are pieces of [Lua](http://www.lua.org/) code that runs in compile-time and 
that can modify or create parts of the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) 
of an okta program. 

As said, macros in okta are Lua code snippets, these code snippets 
can be embedded in okta code, or can be loaded from an external Lua file:

In file: `main.ok`

```
# lua code inside an okta source file
macro my_macro = """
-- Lua code here

return {} -- empty AST
"""

# lua code loaded from file `../eo/example.lua`
# NOTE: Paths are relative to the current okta module 
[[ path = "../eo/example.lua" ]]
macro another_macro
```

All macros in okta have some AST as input and must output another AST.
The input AST is given in the global table `okta.macro.input`.

There are two types of macros in okta: normal macros, and derive macros.

### Normal macros

Normal macros are macros that are called inside functions:

```
macro my_macro = """
return { {
    VarDeclStmt = {
      id = "a",
      value = {
        Int64 = 42
      },
      var_type = "Int64"
    }
} }
"""

fun main(): i64 {
    # this macro generates the AST that would generate `let a = 42;`
    my_macro(); 

    ret a; # magic!
}
```

### Derive macros

Derive macros are executed in type or function declarations, where their input 
is the AST of the type or function declaration in hand. Unlike normal macros, the input AST 
of derive macros cannot be altered, the input type or function declaration will 
remain after the macro gets executed, however, derive macros can generate new AST based on their input. 

```
# this macros returns the input struct but with the name changed
macro example = """
struct = okta.macro.input[1]
struct.StructProto.name = "Pizza" 

return { struct }
"""

# here the type declaration of `Example` isn't modified, 
# but, new AST is created based on it
[[ derive = ("example") ]]
type Example = struct {
    a: i32,
}

fun main() {
    let foo = Example { a = 10 };

    # type declaration for `Pizza` is created in the `example` macro
    let pizza = Pizza { a = 10 };
}
```

### Macro utilities

**TODO**

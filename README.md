# The χ (chi) Programming Language

### Getting Started

The `examples/` folder shows the various the basic syntax and semantics of the language.

Example program (from `examples/recursion.chi`): 
```groovy
extern "<stdio.h>" {
    def printf(fmt_str: &char, ...)
}

def fib(n: int) -> int {
    if n < 2 {
        return 1
    }
    return fib(n-1) + fib(n-2)
}

def main() {
    printf("fib(9) = %i\n", fib(9))
}
```

Try running it:
```console
cargo run -- examples/recursion.chi
```


To compile and run a program, simply pass it as the first argument to the program :
```console
cargo run -- program.chi
```
The generated C code and the compiled executable should be located in the `generated/` folder in the current directory.
Executable generation is only supported on Linux (and perhaps MacOS, untested), though Chi generates a Makefile with the C code in all cases.

### Test
`cargo test` compiles and runs every examples in the `examples/` folder and check if any of them failed to compile or run.
> All examples should compile and return `0`.

### Syntax
> Note: `?sep` will be used to indicate that the block before can be omitted at the last repetition of a `*` or `+` block
```ebnf
program = statement_list

statement_list = ("\n" | ";")* ( statement ("\n" | ";")* )*

statement = def_stmt
          | struct_stmt
          | extern_stmt
          | let_stmt
          | if_stmt
          | while_stmt
          | return_stmt
          | assign_stmt
          | expression

(* module level statements *)
def_stmt = "def" function_head "{" function_body
struct_stmt = "struct" IDENTIFIER "{" ( IDENTIFIER ":" type ","?sep )* "}"
extern_stmt = "extern" STRING "{" extern_body
import_stmt = "import" resource_path
            | "import" "." IDENTIFIER

(* let statements can be used at both the module and function level *)
let_stmt = "let" IDENTIFIER "=" expression

(* function level statements *)
if_stmt = "if" expression "{" function_body
    ( "elif" expression "{" function_body )* 
    ( "else" "{" function_body )?

while_stmt = "while" expression "{" function_body
return_stmt = "return" expression?
assign_stmt = expression "=" expression

function_body = statement_list* "}"
extern_body = "\n"* ( "def" function_head "\n"+ )*
function_head = IDENTIFIER "(" ( IDENTIFIER ":" type ","?sep )* "..."? ")" ("->" type)?
resource_path = IDENTIFIER ("." IDENTIFIER)*

(* expressions *)
expression = equality
equality = comparison ( ("==" | "!=") comparison)*
comparison = term ( (">" | "<" | ">=" | "<=") term )*
term = factor ( ("+" | "-") factor)*
factor = unary ( ("*" | "/") unary)*

unary = ("not" | "+" | "-" | "&" | "*") unary
      | primary

primary = INTEGER
        | FLOAT
        | function_call
        | struct_member
        | struct_init
        | resource_path
        | STRING
        | "true"
        | "false"
        | "null"
        | "(" expression ")"

function_call = resource_path "(" ( expression "," )* ")"
struct_member = primary "." IDENTIFIER
struct_init = IDENTIFIER "{" ( IDENTIFIER ":" expression "," )* "}"

(* type *)
type = IDENTIFIER
     | "&" type
     | "(" type ")"
```

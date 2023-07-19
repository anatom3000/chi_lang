# The χ (chi) programming language

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

(* let statements can be used at both the module and function level *)
let_stmt = "let" IDENTIFIER "=" expression

(* function level statements *)
if_stmt = "if" expression "{" function_body
    ( "elif" expression "{" function_body )* 
    ( "else" "{" function_body )?

while_stmt = "while" expression "{" function_body
return_stmt = "return" expression?
assign_stmt = IDENTIFIER "=" expression

function_body = statement_list* "}"
extern_body = "\n"* ( "def" function_head "\n"+ )*
function_head = IDENTIFIER "(" ( IDENTIFIER ":" type ","?sep )* "..."? ")" ("->" type)?

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
        | IDENTIFIER
        | STRING
        | "true"
        | "false"
        | "null"
        | "(" expression ")"

function_call = IDENTIFIER "(" ( expression "," )* ")"
struct_member = primary "." IDENTIFIER
struct_init = IDENTIFIER "{" ( IDENTIFIER ":" expression "," )* "}"

(* type *)
type = IDENTIFIER
     | "&" type
     | "(" type ")"
```

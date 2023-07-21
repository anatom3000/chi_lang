use crate::{
    analysis::{Type, ExpressionData, ModuleScope, Statement, TypeKind},
    ast::Literal,
};

pub(crate) struct Transpiler {
    lines: Vec<String>,
    header: Vec<String>,
    indent: usize,
}

impl Transpiler {
    pub fn transpile(name: String, scope: ModuleScope) -> Transpiler {
        let mut new: Transpiler = Self {
            lines: vec![],
            header: vec![],
            indent: 0,
        };

        let mut includes = vec![];
        for ext in scope.externs.iter() {
            let source = ext.source.clone();

            if source.starts_with('<') && source.ends_with('>') {
                if includes.contains(&source) {
                    continue;
                }

                includes.push(source);
            } else {
                let source = format!("\"{}\"", source);
                if includes.contains(&source) {
                    continue;
                }

                includes.push(source);
            }
        }

        let guard_name = format!("_{}_H", name.to_uppercase());

        new.add_header_line(format!("#ifndef {guard_name}"));
        new.add_header_line(format!("#define {guard_name}"));
        new.add_new_header_line();

        for inc in includes {
            new.add_line(format!("#include {}", inc));
        }

        new.add_new_line();

        for stmt in scope.statements.iter() {
            new.transpile_statement(&scope, stmt.clone());
        }

        new.add_new_header_line();
        new.add_header_line("#endif".to_string());

        new
    }

    fn add_line(&mut self, line: String) {
        self.lines
            .push(format!("{}{line}", "    ".repeat(self.indent)))
    }

    fn add_new_line(&mut self) {
        self.lines.push(String::new())
    }
    
    fn add_header_line(&mut self, line: String) {
        self.header
            .push(format!("{}{line}", "    ".repeat(self.indent)))
    }

    fn add_new_header_line(&mut self) {
        self.header.push(String::new())
    }

    fn transpile_statement(&mut self, module: &ModuleScope, stmt: Statement) {
        use Statement::*;
        match stmt {
            Expression(expr) => {
                let expr = self.transpile_expression(expr.data);
                self.add_line(format!("{expr};"));
            }
            LetAssignment { name, value } => {
                let expr = self.transpile_expression(value.data);
                let declaration = self.transpile_declaration(value.type_, name);

                self.add_line(format!("{} = {};", declaration, expr));
                // self.add_new_line();
            }
            Assignment { lhs, rhs } => {
                let lhs = self.transpile_expression(lhs.data);
                let rhs = self.transpile_expression(rhs.data);

                self.add_line(format!("{} = {};", lhs, rhs));
                // self.add_new_line();
            }
            FunctionDeclaration { name } => {
                let func = module.get_function(&name).expect("referenced function exists").clone();

                let mut args = func
                    .head
                    .arguments
                    .into_iter()
                    .map(|(name, type_)| self.transpile_declaration(type_, name))
                    .collect::<Vec<String>>()
                    .join(", ");

                if args.is_empty() {
                    args = "void".to_string();
                }

                let declaration = self.transpile_declaration(func.head.return_type, format!("{name}({args})"));
                self.add_header_line(format!("{declaration};"));
                self.add_line(format!("{declaration} {{"));
                self.indent += 1;

                for stmt in func.statements {
                    self.transpile_statement(module, stmt)
                }

                self.indent -= 1;
                //self.lines.pop();
                self.add_line('}'.to_string());
                self.add_new_line();
            },
            StructDeclaration { name } => {
                let TypeKind::Struct {members} = module.get_type(&name).expect("declared struct exists").kind.clone()
                    else { unreachable!("defined struct should have struct type ") };


                self.add_header_line(format!("typedef struct {name} {name};"));
                self.add_line(format!("typdef struct {name} {{"));
                self.indent += 1;
                for (m_name, m_type) in members {
                    let decl = self.transpile_declaration(m_type, m_name);
                    self.add_line(format!("{decl};"));
                }
                self.indent -= 1;

                self.add_line(format!("}} {name};"));
                self.add_new_line();
            },
            If {
                conditions_and_bodies,
                else_body,
            } => {
                let mut first = true;
                for (condition, body) in conditions_and_bodies {
                    let condition = self.transpile_expression(condition.data);
                    if first {
                        first = false;
                        self.add_line(format!("if ({}) {{", condition));
                    } else {
                        self.add_line(format!("}} else if ({}) {{", condition));
                    }

                    self.indent += 1;
                    for stmt in body {
                        self.transpile_statement(module, stmt);
                    }
                    self.indent -= 1;
                }

                if let Some(body) = else_body {
                    self.add_line("} else {".to_string());
                    self.indent += 1;
                    for stmt in body {
                        self.transpile_statement(module, stmt)
                    }
                    self.indent -= 1;
                }

                self.add_line('}'.to_string());
            },
            While { condition, body } => {
                let condition = self.transpile_expression(condition.data);
                self.add_line(format!("while ({}) {{", condition));

                self.indent += 1;
                for stmt in body {
                    self.transpile_statement(module, stmt);
                }
                self.indent -= 1;

                self.add_line('}'.to_string())
            },
            Return(expr) => {
                match expr {
                    Some(expr) => {
                        let expr = self.transpile_expression(expr.data);
                        self.add_line(format!("return {};", expr))
                    },
                    None => self.add_line(format!("return;"))
                };
            }
        }
    }

    fn transpile_expression(&mut self, expr: ExpressionData) -> String {
        match expr {
            ExpressionData::Binary {
                left,
                operator,
                right,
            } => {
                format!(
                    "{} {operator} {}",
                    self.transpile_expression(left.data),
                    self.transpile_expression(right.data)
                )
            }
            ExpressionData::FunctionCall {
                function,
                arguments,
            } => {
                let args = arguments
                    .into_iter()
                    .map(|e| self.transpile_expression(e.data))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{function}({args})")
            }
            ExpressionData::Literal(lit) => match lit {
                Literal::String(content) => format!("\"{}\"", content),
                Literal::Integer { value, .. } => value,
                Literal::Float { value, size } => match size {
                    Some(32) => format!("{}f", value),
                    _ => value
                },
                Literal::Null => "null".to_string(),
                Literal::True => "true".to_string(),
                Literal::False => "false".to_string(),
            },
            ExpressionData::ParenBlock(inner) => {
                format!("({})", self.transpile_expression(inner.data))
            }
            ExpressionData::Unary { operator, argument } => {
                format!("{operator}{}", self.transpile_expression(argument.data))
            }
            ExpressionData::Variable(name) => name,
            ExpressionData::StructMember { instance, member } => format!("{}.{}", self.transpile_expression(instance.data), member),
            ExpressionData::StructInit { name, members } => {
                let members = members.into_iter()
                .map(|(m_name, m_value)| format!(".{m_name} = {}", self.transpile_expression(m_value.data)))
                .collect::<Vec<_>>()
                .join(", ");

                format!("({name}) {{ {members} }}")
            }
        }
    }

    fn transpile_declaration(&mut self, type_: Type, variable: String) -> String {
        match type_ {
            Type::Void => format!("void {variable}", ),
            Type::Identifier(name) => format!("{} {variable}", transpile_primitive_type(name)),
            Type::Reference(inner) => {
                match *inner {
                    Type::Void | Type::Identifier(_) | Type::Reference(_) => self.transpile_declaration(*inner, format!("*{variable}")),
                    // _ => self.transpile_declaration(*inner, format!("(*{variable})"))
                }
            },
            // Type::Array { base, size } => {
            //     match *base {
            //         Type::Void | Type::Identifier(_) /* | Type::Array { .. } */  => self.transpile_declaration(*base, format!("{variable}[{size}]")),
            //         _ => self.transpile_declaration(*base, format!("({variable}[{size}])")),
            //     }
            // },
            // Type::Function { .. } => todo!("function pointer transpile") // for my own sanity
        }
    }
}

fn transpile_primitive_type(from: String) -> String {
    match from.as_str() {
        "int" => "int",
        "float" => "double",
        "bool" => "_Bool",

        "int8" => "int8_t",
        "uint8" => "uint8_t",
        "int16" => "int16_t",
        "uint16" => "uint16_t",
        "int32" => "int32_t",
        "uint32" => "int32_t",
        "int64" => "uint64_t",
        "uint64" => "uint64_t",

        "float32" => "float",
        "float64" => "double",
        "float128" => "long double",

        "usize" => "size_t",
        "isize" => "ptrdiff_t",

        other => other
    }.to_string()
}

impl Transpiler {
    pub fn lines(&self) -> String {
        self.lines.join("\n").trim().to_string()
    }

    pub fn header(&self) -> String {
        self.header.join("\n").trim().to_string()
    }
}

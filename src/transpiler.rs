use std::{collections::{HashMap, HashSet}, fmt::Display};

use crate::{
    analysis::{ExpressionData, ModuleScope, ResourceKind, Statement, Type, TypeKind, get_global_resource},
    ast::Literal,
};

pub(crate) struct ModuleTranspiler<'a> {
    indent: usize,
    scope: &'a ModuleScope,
    shadowed_variables: HashMap<String, usize>,
    includes: HashSet<String>,
    global_variables: Vec<String>,
    functions: Vec<(String, String)>, // declaration-body pairs
    structs: Vec<(String, String)>, // name-body pairs
}

impl<'a> ModuleTranspiler<'a> {

    pub fn transpile(
        scope: &'a ModuleScope,
    ) -> ModuleTranspiler { 
        let mut new = ModuleTranspiler {
            indent: 0,
            scope,
            shadowed_variables: HashMap::new(),
            includes: HashSet::from([
                "<stddef.h>".to_string(),
                "<stdbool.h>".to_string(),
                "<stdint.h>".to_string(),
            ]),
            global_variables: vec![],
            functions: vec![],
            structs: vec![]
        };

        // TODO: lazily add includes (e.g. include stddef.h only when NULL is used)
        for ext in &new.scope.externs {
            new.includes.insert(if ext.starts_with('<') && ext.ends_with('>') { ext.clone() } else { format!("\"{}\"", ext) });
        }

        for (method, impls) in &new.scope.declared_methods {
            for (receiver, (impl_module, func)) in impls {
                let args = func
                    .head
                    .arguments
                    .iter()
                    .map(|(name, type_)| {
                        new.shadowed_variables.insert(name.clone(), 0);
                        new.transpile_declaration(type_, name)
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                // args is never empty since it has at least a receiver

                let name = new.transpile_method_path(receiver, method, impl_module);
                let declaration = new
                    .transpile_declaration(&func.head.return_type, &format!("{name}({args})"));

                let mut code = vec![];
                new.indent += 1;
                for stmt in &func.statements {
                    code.push(new.transpile_statement(stmt))
                }
                new.shadowed_variables.clear();
                new.indent -= 1;
                //new.source.pop();
                new.functions.push((declaration, code.join("\n")))
            }
        }

        for stmt in &new.scope.statements {
            match stmt {
                Statement::FunctionDeclaration { name } => {
                    let func = new
                        .scope
                        .get_function(&name)
                        .expect("declared function exists")
                        .clone();
    
                    let mut args = func
                        .head
                        .arguments
                        .iter()
                        .map(|(name, type_)| {
                            new.shadowed_variables.insert(name.clone(), 0);
                            new.transpile_declaration(type_, name)
                        })
                        .collect::<Vec<String>>()
                        .join(", ");
    
                    if args.is_empty() {
                        args = "void".to_string();
                    }
    
                    // don't "namespacify" the function name if function is the main function or an extern function
                    let func_name = new.transpile_path(&new.scope.make_path_absolute(vec![name.clone()]).expect("referenced path exists"));
                    let declaration = new
                        .transpile_declaration(&func.head.return_type, &format!("{func_name}({args})"));

                    let mut code = vec![];
                    new.indent += 1;
                    for stmt in &func.statements {
                        code.push(new.transpile_statement(stmt))
                    }
                    new.shadowed_variables.clear();
                    new.indent -= 1;
                    //new.source.pop();
                    new.functions.push((declaration, code.join("\n")))
                }
                Statement::StructDeclaration { name } => {
                    let struct_type = new
                        .scope
                        .get_type(&vec![name.clone()])
                        .expect("declared struct exists");
    
                    let TypeKind::Struct {members} = struct_type.kind.clone()
                        else { unreachable!("defined struct should have struct type ") };
    
                    let struct_name = new.transpile_path(&new.scope.make_path_absolute(vec![name.clone()]).expect("referenced path exists"));

                    let mut body = vec![];
                    new.indent += 1;
                    for (m_name, m_type) in members {
                        let decl = new.transpile_declaration(&m_type, &m_name);
                        body.push(new.add_line(format!("{decl};")));
                    }
                    new.indent -= 1;
                    
                    new.structs.push((struct_name, body.join("\n")));
                }
                Statement::LetAssignment { .. } => todo!("global variable transpilation"),
                Statement::Import(child) => {
                    let submodule = match &new.scope.resources.get(child).expect("imported submodule is valid").kind {
                        ResourceKind::Module(ref m) => m,
                        _ => unreachable!("imported submodule is a resource of type Module")
                    };
                    let mut transpiled_submodule = ModuleTranspiler::transpile(submodule);

                    new.includes.extend(transpiled_submodule.includes.drain());
                    new.global_variables.append(&mut transpiled_submodule.global_variables);
                    new.functions.append(&mut transpiled_submodule.functions);
                    new.structs.append(&mut transpiled_submodule.structs);

                },
                Statement::Expression(_)
                | Statement::If { .. }
                | Statement::Assignment { .. }
                | Statement::Return(_)
                | Statement::While { .. } => unreachable!("those statements cannot exist in a module scope")
            }
        }

        new
    }

    fn add_line(&mut self, line: String) -> String {
        format!("{}{line}", "    ".repeat(self.indent))
    }

    fn transpile_statement(&mut self, stmt: &Statement) -> String {
        use Statement::*;
        match stmt {
            Expression(expr) => {
                let expr = self.transpile_expression(&expr.data);
                self.add_line(format!("{expr};"))
            }
            LetAssignment { name, value } => {
                let expr = self.transpile_expression(&value.data);
                let name = self.transpile_new_variable(name);
                let declaration = self.transpile_declaration(&value.type_, &name);

                self.add_line(format!("{} = {};", declaration, expr))
            }
            Assignment { lhs, rhs } => {
                let lhs = self.transpile_expression(&lhs.data);
                let rhs = self.transpile_expression(&rhs.data);

                self.add_line(format!("{} = {};", lhs, rhs))
            }
            If { conditions_and_bodies, else_body, } => {
                let mut code = vec![];

                let mut first = true;
                for (condition, body) in conditions_and_bodies {
                    let condition = self.transpile_expression(&condition.data);
                    if first {
                        first = false;
                        code.push(self.add_line(format!("if ({}) {{", condition)));
                    } else {
                        code.push(self.add_line(format!("}} else if ({}) {{", condition)));
                    }

                    self.indent += 1;
                    for stmt in body {
                        code.push(self.transpile_statement(stmt));
                    }
                    self.indent -= 1;
                }

                if let Some(body) = else_body {
                    self.add_line("} else {".to_string());
                    self.indent += 1;
                    for stmt in body {
                        code.push(self.transpile_statement(stmt));
                    }
                    self.indent -= 1;
                }

                code.push(self.add_line('}'.to_string()));

                code.join("\n")
            }
            While { condition, body } => {
                let mut code = vec![];

                let condition = self.transpile_expression(&condition.data);
                code.push(self.add_line(format!("while ({}) {{", condition)));

                self.indent += 1;
                for stmt in body {
                    code.push(self.transpile_statement(stmt));
                }
                self.indent -= 1;

                code.push(self.add_line('}'.to_string()));

                code.join("\n")
            }
            Return(expr) => {
                match expr {
                    Some(expr) => {
                        let expr = self.transpile_expression(&expr.data);
                        self.add_line(format!("return {};", expr))
                    }
                    None => self.add_line(format!("return;")),
                }
            },
            StructDeclaration { .. }
            | FunctionDeclaration { .. }
            | Import(_) => unreachable!("those statements cannot exist in a function scope")
        }
    }

    fn transpile_expression(&mut self, expr: &ExpressionData) -> String {
        match expr {
            ExpressionData::Binary { left, operator, right, } => {
                format!(
                    "{} {operator} {}",
                    self.transpile_expression(&left.data),
                    self.transpile_expression(&right.data)
                )
            }
            ExpressionData::FunctionCall { function, arguments, } => {

                let head = match get_global_resource(&function, None) {
                    Ok(ResourceKind::Function(func)) => func,
                    _ => unreachable!("called function exists"),
                };
                let name = if head.is_variadic.is_some() {
                    function.last().expect("path is not empty").clone()
                } else {
                    self.transpile_path(function)
                };
                let args = arguments
                    .into_iter()
                    .map(|e| self.transpile_expression(&e.data))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}({args})")
            }
            ExpressionData::MethodCall { receiver, method, arguments } => {
                let (impl_module, _) = self.scope.declared_methods
                    .get(method)
                        .expect("called method name exists")
                    .get(&receiver)
                        .expect("called method on type exists");
                
                let name = self.transpile_method_path(&receiver, &method, impl_module);
                let args = arguments
                    .into_iter()
                    .map(|e| self.transpile_expression(&e.data))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}({args})")
            },
            ExpressionData::Literal(lit) => match lit {
                Literal::String(content) => format!("\"{}\"", content),
                Literal::Integer { value, .. } => value.clone(),
                Literal::Float { value, size } => match size {
                    Some(32) => format!("{}f", value),
                    _ => value.clone(),
                },
                Literal::Null => "NULL".to_string(),
                Literal::True => "true".to_string(),
                Literal::False => "false".to_string(),
            },
            ExpressionData::ParenBlock(inner) => {
                format!("({})", self.transpile_expression(&inner.data))
            }
            ExpressionData::Unary { operator, argument } => {
                format!("{operator}{}", self.transpile_expression(&argument.data))
            }
            ExpressionData::Variable(path) => {
                if path.len() == 1 {
                    self.transpile_variable(&path[0])
                } else {
                    self.transpile_path(&path)
                }
                
            },
            ExpressionData::StructMember { instance, member } => {
                format!("{}.{}", self.transpile_expression(&instance.data), member)
            }
            ExpressionData::StructInit { path, members } => {
                let members = members
                    .into_iter()
                    .map(|(m_name, m_value)| {
                        format!(".{m_name} = {}", self.transpile_expression(&m_value.data))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({}) {{ {members} }}", self.transpile_named_type(&path))
            }
        }
    }

    fn transpile_declaration(&mut self, type_: &Type, variable: &String) -> String {
        match type_ {
            Type::Void => format!("void {variable}",),
            Type::Path(path) if path[0] == "str" => {
                // hack until a real string type is implemented
                format!("char *{variable}")
            },
            Type::Path(path) => format!("{} {variable}", self.transpile_named_type(&path)),
            Type::Reference { inner, mutable: _ } => {
                match **inner {
                    Type::Void | Type::Path(_) | Type::Reference{..} => {
                        self.transpile_declaration(inner, &format!("*{variable}"))
                    }
                    // _ => self.transpile_declaration(*inner, format!("(*{variable})"))
                }
            }
            // Type::Array { base, size } => {
            //     match *base {
            //         Type::Void | Type::Identifier(_) /* | Type::Array { .. } */  => self.transpile_declaration(*base, format!("{variable}[{size}]")),
            //         _ => self.transpile_declaration(*base, format!("({variable}[{size}])")),
            //     }
            // },
            // Type::Function { .. } => todo!("function pointer transpile") // for my own sanity
        }
    }

    fn transpile_variable(&self, name: &String) -> String {
        let n = self.shadowed_variables.get(name).expect("used variables should be declared").clone();
        format!("{name}{}", "_".repeat(n))
    }

    fn transpile_new_variable(&mut self, name: &String) -> String {
        let n = match self.shadowed_variables.get(name).cloned() {
            Some(n) => n+1,
            None => 0
        };
        self.shadowed_variables.insert(name.clone(), n);
        self.transpile_variable(name)
    }

    fn transpile_path(&self, path: &Vec<String>) -> String {
        // TODO: check for conflicting function names
        path.join("_")
    }

    fn transpile_named_type(&self, path: &Vec<String>) -> String {
        if path.len() == 1 {
            match path[0].as_str() {
                "int" => "int".to_string(),
                "uint" => "unsigned int".to_string(),
                "float" => "double".to_string(),
                "bool" => "_Bool".to_string(),
                "char" => "char".to_string(),

                "int8" => "int8_t".to_string(),
                "uint8" => "uint8_t".to_string(),
                "int16" => "int16_t".to_string(),
                "uint16" => "uint16_t".to_string(),
                "int32" => "int32_t".to_string(),
                "uint32" => "int32_t".to_string(),
                "int64" => "uint64_t".to_string(),
                "uint64" => "uint64_t".to_string(),

                "float32" => "float".to_string(),
                "float64" => "double".to_string(),
                "float128" => "long double".to_string(),

                "usize" => "size_t".to_string(),
                "isize" => "ptrdiff_t".to_string(),

                _ => self.transpile_path(path),
            }
        } else {
            self.transpile_path(path)
        }
    }

    fn transpile_type(&self, ty: &Type) -> String {
        match ty {
            Type::Void => "void".to_string(),
            Type::Path(path) => self.transpile_named_type(path),
            Type::Reference { inner, mutable } => {
                if *mutable {
                    format!("_mutref__{}", self.transpile_type(inner))
                } else {
                    format!("_ref__{}", self.transpile_type(inner))
                }
            }
        }
    }

    fn transpile_method_path(&self, receiver: &Type, method: &String, impl_module: &Vec<String>) -> String {
        format!("{}__{}_{}", self.transpile_type(receiver), self.transpile_path(&impl_module), method)
    }


}

impl<'a> Display for ModuleTranspiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut something = false;
        
        for inc in &self.includes {
            something = true;
            writeln!(f, "#include {inc}")?;
        }

        if something { writeln!(f)?; }
        something = false;

        for (name, _) in &self.structs {
            something = true;

            writeln!(f, "typedef struct {name} {name};")?;
        }

        if something { writeln!(f)?; }
        something = false;

        for (decl, _) in &self.functions {
            something = true;

            writeln!(f, "{decl};")?;
        }

        if something { writeln!(f)?; }
        something = false;

        for var in &self.global_variables {
            something = true;

            writeln!(f, "{var};")?;
        }

        if something { writeln!(f)?; }
        something = false;

        for (name, body) in &self.structs {
            something = true;

            writeln!(f, "struct {name} {{\n{body}\n}};\n")?;
        }

        if something { writeln!(f)?; }

        for (decl, body) in &self.functions {
            writeln!(f, "{decl} {{\n{body}\n}}\n")?;
        }

        Ok(())
    }
}

use std::{path::PathBuf, collections::HashMap, mem};

use crate::{
    analysis::{Type, ExpressionData, ModuleScope, Statement, TypeKind, Resource},
    ast::Literal,
};

pub(crate) struct ModuleTranspiler {
    pub(crate) source_path: PathBuf,
    pub(crate) header_path: PathBuf,
    source: Vec<String>,
    header: Vec<String>,
    indent: usize,
    namespace: Vec<String>,
    scope: ModuleScope,
    is_main: bool
}

impl ModuleTranspiler {
    pub fn transpile(path: Vec<String>, scope: ModuleScope, transpiled_modules: &mut HashMap<Vec<String>, ModuleTranspiler>, is_main: bool) {

        if transpiled_modules.contains_key(&path) {
            for (_, res) in scope.resources {
                if let Resource::Module(m) = res {
                    ModuleTranspiler::transpile(m.path, m.scope.expect("analysis called before transpilation"), transpiled_modules, false)
                }
            }
            return;
        }
        
        let piter = if is_main {
            path.iter()
        } else {
            let mut p = path.iter();
            p.next();
            p
        };

        let mut source_path: PathBuf = piter.collect();
        let mut header_path: PathBuf = source_path.clone();

        source_path.set_extension("c");
        header_path.set_extension("h");


        let mut new = ModuleTranspiler {
            source_path,
            header_path,
            source: vec![],
            header: vec![],
            indent: 0,
            namespace: path.clone(),
            scope,
            is_main
        };

        let mut includes = vec![];
        for ext in mem::take(&mut new.scope.externs) {
            if ext.starts_with('<') && ext.ends_with('>') {
                if includes.contains(&ext) {
                    continue;
                }

                includes.push(ext);
            } else {
                let ext = format!("\"{}\"", ext);
                if includes.contains(&ext) {
                    continue;
                }

                includes.push(ext);
            }
        }

        let guard_name = format!("_{}_H", path.iter().map(|x| x.to_uppercase()).collect::<Vec<_>>().join("_"));

        new.add_header_line(format!("#ifndef {guard_name}"));
        new.add_header_line(format!("#define {guard_name}"));
        new.add_new_header_line();

        for inc in includes {
            new.add_header_line(format!("#include {}", inc));
        }

        new.add_line(format!("#include \"{}\"", new.header_path.display()));
        

        new.add_new_line();

        for stmt in mem::take(&mut new.scope.statements) {
            new.transpile_statement(stmt.clone());
        }

        new.add_new_header_line();
        new.add_header_line("#endif".to_string());

        let resources = mem::take(&mut new.scope.resources);

        transpiled_modules.insert(path, new);

        for (_, res) in resources {
            if let Resource::Module(m) = res {
                ModuleTranspiler::transpile(m.path, m.scope.expect("analysis called before transpilation"), transpiled_modules, false)
            }
        }
    }

    fn add_line(&mut self, line: String) {
        self.source
            .push(format!("{}{line}", "    ".repeat(self.indent)))
    }

    fn add_new_line(&mut self) {
        self.source.push(String::new())
    }
    
    fn add_header_line(&mut self, line: String) {
        self.header
            .push(format!("{}{line}", "    ".repeat(self.indent)))
    }

    fn add_new_header_line(&mut self) {
        self.header.push(String::new())
    }

    fn transpile_statement(&mut self, stmt: Statement) {
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
                let func = self.scope.get_function(&name).expect("declared function exists").clone();
                
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

                // don't "namespacify" the function name if function is the main function or an extern function
                let func_name = if self.is_main && name == "main" { name } 
                                        else { self.transpile_path(&self.to_absolute_path(vec![name])) };
                let declaration = self.transpile_declaration(func.head.return_type, format!("{func_name}({args})"));
                self.add_header_line(format!("{declaration};"));
                self.add_line(format!("{declaration} {{"));
                self.indent += 1;

                for stmt in func.statements {
                    self.transpile_statement(stmt)
                }

                self.indent -= 1;
                //self.source.pop();
                self.add_line('}'.to_string());
                self.add_new_line();
            },
            StructDeclaration { name } => {

                let TypeKind::Struct {members} = self.scope.get_type(&vec![name.clone()]).expect("declared struct exists").kind.clone()
                    else { unreachable!("defined struct should have struct type ") };

                let struct_name = self.transpile_path(&self.to_absolute_path(vec![name]));
                self.add_header_line(format!("typedef struct {struct_name} {struct_name};"));
                self.add_line(format!("typedef struct {struct_name} {{"));
                self.indent += 1;
                for (m_name, m_type) in members {
                    let decl = self.transpile_declaration(m_type, m_name);
                    self.add_line(format!("{decl};"));
                }
                self.indent -= 1;

                self.add_line(format!("}} {struct_name};"));
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
                        self.transpile_statement(stmt);
                    }
                    self.indent -= 1;
                }

                if let Some(body) = else_body {
                    self.add_line("} else {".to_string());
                    self.indent += 1;
                    for stmt in body {
                        self.transpile_statement(stmt)
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
                    self.transpile_statement(stmt);
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
            },
            Import(path) => {
                self.add_header_line(format!("#include \"{}.h\"", path[1..].join("/")))
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
                let head = self.scope.get_function_head(&function).expect("called function exists");
                let function = if head.is_variadic.is_some() { function.last().expect("path is not empty").clone() } 
                                   else {self.transpile_path(&function)};
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
            ExpressionData::Variable(path) => self.transpile_path(&path),
            ExpressionData::StructMember { instance, member } => format!("{}.{}", self.transpile_expression(instance.data), member),
            ExpressionData::StructInit { path, members } => {
                let members = members.into_iter()
                .map(|(m_name, m_value)| format!(".{m_name} = {}", self.transpile_expression(m_value.data)))
                .collect::<Vec<_>>()
                .join(", ");

                format!("({}) {{ {members} }}", self.transpile_type(path))
            }
        }
    }

    fn transpile_declaration(&mut self, type_: Type, variable: String) -> String {
        match type_ {
            Type::Void => format!("void {variable}", ),
            Type::Path(path) => format!("{} {variable}", self.transpile_type(path)),
            Type::Reference(inner) => {
                match *inner {
                    Type::Void | Type::Path(_) | Type::Reference(_) => self.transpile_declaration(*inner, format!("*{variable}")),
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

    fn transpile_path(&self, path: &Vec<String>) -> String {
        // TODO: check for conflicting function names
        path.join("_")
    }

    fn to_absolute_path(&self, mut path: Vec<String>) -> Vec<String> {
        let mut ns = self.namespace.clone();
        ns.append(&mut path);
        ns
    }

    fn transpile_type(&self, path: Vec<String>) -> String {
        if path.len() == 1 {
            match path[0].as_str() {
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
    
        } else {
            self.transpile_path(&self.to_absolute_path(path))
        }
    }
    
}


impl ModuleTranspiler {
    pub fn source(&self) -> String {
        self.source.join("\n").trim().to_string()
    }

    pub fn header(&self) -> String {
        self.header.join("\n").trim().to_string()
    }
}

pub(crate) fn generate_makefile(module_name: &str, files: &[String]) -> String {
    // base makefile credit: https://makefiletutorial.com/#makefile-cookbook
    format!("
TARGET_EXEC := {}
BUILD_DIR := ./build

SRCS := {}
TARGET_EXEC := final_program

BUILD_DIR := ./build
SRC_DIRS := ./src

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

# String substitution (suffix version without %).
# As an example, ./build/hello.cpp.o turns into ./build/hello.cpp.d
DEPS := $(OBJS:.o=.d)

INC_DIRS := .
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CPPFLAGS := $(INC_FLAGS) -MMD -MP

$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@ $(LDFLAGS)

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)

-include $(DEPS)
",
    module_name, files.join(" "))
}
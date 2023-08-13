use std::collections::HashMap;
use std::fmt::Display;
use std::vec;

use crate::ast::{self, Expression, Import, Literal, Statement, Type};
use crate::lexer::{self, Token, TokenData};

#[derive(Debug, Clone)]
pub enum ParserError {
    LexingError {
        line: usize,
        column: usize,
        error: lexer::LexingError,
    },
    Expected {
        rule: &'static str,
        found: Option<Token>,
    },
    #[allow(dead_code)]
    ExpectedWithHelp {
        rule: &'static str,
        help: &'static str,
        found: Option<Token>,
    },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexingError { line, column, error } => write!(f, "Lexing error at {line}:{column} - {error:?}"),
            Self::Expected { rule, found } => match found {
                                                                                            // name is hardcoded for easier debugging (vscode integration)
                Some(token) => write!(f, "Unexpected token: expected {}, found `{}` at src/program.chi:{}:{}", rule, token.data, token.line, token.column),
                None => write!(f, "Unexpected token: expected {}, found end of file", rule),
            },
            Self::ExpectedWithHelp { rule, help, found } => match found {
                                                                                            // name is hardcoded for easier debugging (vscode integration)
                Some(token) => write!(f, "Unexpected token: expected {}, found `{}` at src/program.chi:{}:{}\n\t- help: {}", rule, token.data, token.line, token.column, help),
                None => write!(f, "Unexpected token: expected {}, found end of file\n\t- help: {}", rule, help),
            },
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub errors: Vec<ParserError>,
    current: usize,
    no_struct_init: bool,
}

macro_rules! binary_operator {
    ($name:ident, $tokens:pat, $child:ident) => {
        fn $name(&mut self) -> Option<ast::Expression> {
            let mut expr = self.$child()?;

            loop {
                match self.current_token() {
                    Some($tokens) => {
                        let operator = self.current_token().expect("current token should exist");
                        self.current += 1;
                        expr = Expression::Binary {
                            left: Box::new(expr),
                            operator,
                            right: Box::new(self.$child()?),
                        };
                    }
                    _ => break,
                }
            }
            Some(expr)
        }
    };
}

// general methods and constructor
impl Parser {
    pub fn from_source(input: &str) -> Self {
        let lexed = lexer::Lexer::new(input).lex();

        Parser {
            tokens: lexed.tokens,
            current: 0,
            no_struct_init: false,
            errors: lexed
                .errors
                .iter()
                .map(|x| {
                    let position = *x.0;
                    let error = x.1.clone();
                    ParserError::LexingError {
                        line: position.0,
                        column: position.1,
                        error,
                    }
                })
                .collect(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement>, Vec<ParserError>> {
        match self.statement_list() {
            Some(ast) => Ok(ast),
            None => Err(self.errors),
        }
    }

    fn current_token(&self) -> Option<TokenData> {
        Some(self.tokens.get(self.current)?.data.clone())
    }

    fn current_token_no_whitespace(&mut self) -> Option<TokenData> {
        loop {
            match self.tokens.get(self.current)?.data.clone() {
                TokenData::NewLine => self.current += 1,
                token => return Some(token),
            }
        }
    }

    fn full_current_token(&self) -> Option<Token> {
        self.tokens.get(self.current).cloned()
    }
}

// expression parser
impl Parser {
    fn expression(&mut self) -> Option<ast::Expression> {
        self.equality()
    }

    binary_operator!(equality, TokenData::Equal | TokenData::NotEqual, comparison);
    binary_operator!(
        comparison,
        TokenData::Greater
            | TokenData::Lesser
            | TokenData::GreatorOrEqual
            | TokenData::LesserOrEqual,
        term
    );
    binary_operator!(term, TokenData::Plus | TokenData::Minus, factor);
    binary_operator!(factor, TokenData::Star | TokenData::Slash, unary);

    fn unary(&mut self) -> Option<Expression> {
        match self.current_token() {
            Some(
                TokenData::Not
                | TokenData::Plus
                | TokenData::Minus
                | TokenData::Ref
                | TokenData::MutRef
                | TokenData::Star,
            ) => {
                let operator = self.current_token().expect("current token should be valid");
                self.current += 1;
                return Some(Expression::Unary {
                    operator,
                    argument: Box::new(self.unary()?),
                });
            }
            None => {
                self.errors.push(ParserError::Expected {
                    rule: "unary",
                    found: None,
                });
                return None;
            }
            _ => {}
        }

        self.primary()
    }

    fn primary(&mut self) -> Option<Expression> {
        let mut expr = match self.current_token() {
            None => {
                self.errors.push(ParserError::Expected {
                    rule: "expression",
                    found: None,
                });
                return None;
            }
            Some(token) => {
                self.current += 1;
                match token {
                    TokenData::Integer(value, signed_and_size) => {
                        Expression::Literal(self.integer(value, signed_and_size))
                    }
                    TokenData::Float(value, size) => Expression::Literal(Literal::Float {
                        value: value,
                        size: size,
                    }),
                    TokenData::Identifier(_) => self.identifier_primary()?,
                    TokenData::String(content) => {
                        Expression::Literal(ast::Literal::String(content))
                    }

                    TokenData::True => Expression::Literal(ast::Literal::True),
                    TokenData::False => Expression::Literal(ast::Literal::False),
                    TokenData::Null => Expression::Literal(ast::Literal::Null),
                    TokenData::LeftParen => {
                        let expr = self.expression()?;

                        match self.current_token() {
                            Some(TokenData::RightParen) => {
                                self.current += 1;
                                Expression::ParenBlock(Box::new(expr))
                            }
                            _ => {
                                self.errors.push(ParserError::Expected {
                                    rule: "')' closing expression paren block",
                                    found: self.full_current_token(),
                                });
                                return None;
                            }
                        }
                    }
                    _ => {
                        self.current -= 1;
                        // token
                        self.errors.push(ParserError::Expected {
                            rule: "expression",
                            found: self.full_current_token(),
                        });
                        return None;
                    }
                }
            }
        };

        loop {
            match self.current_token() {
                Some(TokenData::Dot) => {
                    self.current += 1;
                    match self.current_token() {
                        Some(TokenData::Identifier(member)) => {
                            self.current += 1;
                            expr = Expression::StructMember {
                                instance: Box::new(expr),
                                member,
                            }
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "identifier after `.`",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn identifier_primary(&mut self) -> Option<Expression> {
        self.current -= 1;
        let path = self.resource_path()?;
        Some(match self.current_token() {
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let args = self.function_call_arguments()?;
                Expression::FunctionCall {
                    function: path,
                    arguments: args,
                }
            }
            Some(TokenData::LeftBrace) if !self.no_struct_init => {
                self.current += 1;

                let mut members = HashMap::new();
                loop {
                    match self.current_token_no_whitespace() {
                        Some(TokenData::RightBrace) => {
                            self.current += 1;
                            break;
                        }
                        Some(TokenData::Identifier(m_name)) => {
                            self.current += 1;
                            match self.current_token() {
                                Some(TokenData::Colon) => {
                                    self.current += 1;
                                    members.insert(m_name, self.expression()?);
                                    match self.current_token_no_whitespace() {
                                        Some(TokenData::Comma) => self.current += 1,
                                        Some(TokenData::RightBrace) => {
                                            self.current += 1;
                                            break;
                                        }
                                        _ => {
                                            self.errors.push(ParserError::Expected {
                                                rule: "'}' (end of struct initialization) or ',' (member seperator)",
                                                found: self.full_current_token(),
                                            });
                                            return None;
                                        }
                                    }
                                }
                                _ => {
                                    self.errors.push(ParserError::Expected {
                                        rule: "`:` after member name in struct initialization",
                                        found: self.full_current_token(),
                                    });
                                    return None;
                                }
                            }
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "member name or `}` in struct initialization",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }

                Expression::StructInit { path, members }
            }

            _ => Expression::Variable(path),
        })
    }

    fn function_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = vec![];

        if let Some(TokenData::RightParen) = self.current_token() {
            self.current += 1;
            return Some(args);
        }

        loop {
            args.push(self.expression()?);
            match self.current_token() {
                Some(TokenData::Comma) => self.current += 1,
                Some(TokenData::RightParen) => {
                    self.current += 1;
                    return Some(args);
                }
                _ => {
                    self.errors.push(ParserError::Expected {
                        rule: "')' (end of arguments) or ',' (argument seperator)",
                        found: self.full_current_token(),
                    });
                    return None;
                }
            }
        }
    }

    fn integer(
        &mut self,
        value: String,
        signed_and_size: Option<(bool, Option<usize>)>,
    ) -> Literal {
        match signed_and_size {
            Some((signed, size)) => Literal::Integer {
                value,
                signed,
                size,
            },
            None => Literal::Integer {
                value,
                signed: true,
                size: None,
            },
        }
    }
}

// type parser
impl Parser {
    fn _def_type_args(&mut self) -> Option<Vec<Type>> {
        let mut args = vec![];

        if let Some(TokenData::RightParen) = self.current_token() {
            self.current += 1;
            return Some(args);
        }

        loop {
            args.push(self.type_()?);
            match self.current_token() {
                Some(TokenData::Comma) => self.current += 1,
                Some(TokenData::RightParen) => {
                    self.current += 1;
                    return Some(args);
                }
                _ => {
                    self.errors.push(ParserError::Expected {
                        rule: "')' (end of arguments in def type) or ',' (argument seperator in def type)",
                        found: self.full_current_token(),
                    });
                    return None;
                }
            }
        }
    }

    fn type_(&mut self) -> Option<Type> {
        let ty = match self.current_token() {
            Some(TokenData::Identifier(_)) => Type::Path(self.resource_path()?),
            Some(TokenData::Ref) => {
                self.current += 1;
                Type::Reference {
                    inner: Box::new(self.type_()?),
                    mutable: false,
                }
            },
            Some(TokenData::MutRef) => {
                self.current += 1;
                Type::Reference {
                    inner: Box::new(self.type_()?),
                    mutable: true,
                }
            },
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let ty = self.type_()?;
                match self.current_token() {
                    Some(TokenData::RightParen) => {
                        self.current += 1;
                        ty
                    }
                    _ => {
                        self.errors.push(ParserError::Expected {
                            rule: "')' closing type paren block",
                            found: self.full_current_token(),
                        });
                        return None;
                    }
                }
            }
            // Some(TokenData::Def) => {
            //     self.current += 1;
            //     match self.current_token() {
            //         Some(TokenData::LeftParen) => {
            //             self.current += 1;
            //             let arguments = self.def_type_args()?;
            //             match self.current_token() {
            //                 Some(TokenData::Arrow) => {
            //                     self.current += 1;
            //                     let return_type = self.type_()?;
            //                     Type::Function { return_type: Box::new(Some(return_type)), arguments }
            //                 },
            //                 _ => {
            //                     Type::Function { return_type: Box::new(None), arguments }
            //                 }
            //             }
            //         },
            //         _ => {
            //             self.errors.push(ParserError::Expected { rule: "'(' after 'def' type", found: self.full_current_token() });
            //             return None
            //         }
            //     }
            // },
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "type",
                    found: self.full_current_token(),
                });
                return None;
            }
        };

        // TODO: implement back array types
        // while let Some(TokenData::LeftBracket) = self.current_token() {
        //     self.current += 1;
        //     match self.current_token() {
        //         Some(TokenData::Number(value)) => {
        //             self.current += 1;
        //             match self.current_token() {
        //                 Some(TokenData::RightBracket) => {
        //                     self.current += 1;
        //                     ty = Type::Array { base: Box::new(ty), size: value }
        //                 },
        //                 _ => {
        //                     self.errors.push(ParserError::Expected { rule: "']' closing array type", found: self.full_current_token() });
        //                     return None;
        //                 }
        //             }
        //         },
        //         Some(TokenData::RightBracket) => {
        //             self.errors.push(ParserError::ExpectedWithHelp { rule: "integer literal", help: "unsized arrays are not supported (yet)", found: self.full_current_token() });
        //             return None;
        //         },
        //         _ => {
        //             self.errors.push(ParserError::Expected { rule: "integer literal", found: self.full_current_token() });
        //             return None;
        //         }
        //     }
        // }

        Some(ty)
    }
}

// statement parser
impl Parser {
    fn statement_list(&mut self) -> Option<Vec<Statement>> {
        let mut statements = vec![];

        // handle starting newlines or semicolons
        'consume_separator: loop {
            match self.current_token() {
                Some(TokenData::NewLine) => self.current += 1,
                Some(TokenData::Semicolon) => self.current += 1,
                _ => break 'consume_separator,
            }
        }

        while self.current_token().is_some() {
            let stmt = self.statement()?;

            // dbg!(&stmt);
            // dbg!(self.current_token_no_whitespace());
            // panic!();

            statements.push(stmt);

            let mut first = true;
            'consume_separator: loop {
                match self.current_token() {
                    Some(TokenData::NewLine) => self.current += 1,
                    None => return Some(statements),
                    _ => {
                        if first {
                            self.errors.push(ParserError::Expected {
                                rule: "new line or `;` (module)",
                                found: self.full_current_token(),
                            })
                        } else {
                            break 'consume_separator;
                        }
                    }
                }
                first = false;
            }
        }

        Some(statements)
    }

    fn statement(&mut self) -> Option<Statement> {
        match self.current_token_no_whitespace() {
            Some(TokenData::Let) => {
                self.current += 1;
                self.let_stmt()
            }
            Some(TokenData::Def) => {
                self.current += 1;
                self.def_stmt()
            }
            Some(TokenData::Extern) => {
                self.current += 1;
                self.extern_stmt()
            }
            Some(TokenData::If) => {
                self.current += 1;
                self.if_stmt()
            }
            Some(TokenData::Struct) => {
                self.current += 1;
                self.struct_stmt()
            }
            Some(TokenData::While) => {
                self.current += 1;
                self.while_stmt()
            }
            Some(TokenData::Return) => {
                self.current += 1;
                self.return_stmt()
            }
            Some(TokenData::Import) => {
                self.current += 1;
                self.import_stmt()
            }
            Some(_) => {
                let expr = self.expression()?;
                self.expr_stmt(expr)
            }
            None => {
                self.errors.push(ParserError::Expected {
                    rule: "statement",
                    found: self.full_current_token(),
                });
                None
            }
        }
    }

    fn expr_stmt(&mut self, expr: Expression) -> Option<Statement> {
        Some(match self.current_token() {
            Some(TokenData::Assign) => {
                self.current += 1;
                let rhs = self.expression()?;
                Statement::Assignment { lhs: expr, rhs }
            }
            _ => Statement::Expression(expr),
        })
    }

    fn let_stmt(&mut self) -> Option<Statement> {
        if let Some(TokenData::Identifier(name)) = self.current_token() {
            self.current += 1;
            if let Some(TokenData::Assign) = self.current_token() {
                self.current += 1;
                return Some(Statement::LetAssignment {
                    name,
                    value: self.expression()?,
                });
            } else {
                self.errors.push(ParserError::Expected {
                    rule: "'='",
                    found: self.full_current_token(),
                })
            }
        } else {
            self.errors.push(ParserError::Expected {
                rule: "identifier",
                found: self.full_current_token(),
            })
        }

        None
    }

    fn def_stmt(&mut self) -> Option<Statement> {
        let (name, arguments, return_type, is_variadic) = self.function_head()?;

        match self.current_token() {
            Some(TokenData::LeftBrace) => self.current += 1,
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "'{' (function body) or '->' (return type)",
                    found: self.full_current_token(),
                });
                return None;
            }
        }
        let body = self.function_body()?;

        Some(Statement::FunctionDeclaration {
            name,
            return_type,
            arguments,
            body,
            is_variadic,
        })
    }

    fn extern_def_stmt(&mut self) -> Option<Statement> {
        let (name, arguments, return_type, is_variadic) = self.function_head()?;

        Some(Statement::ExternFunctionDeclaration {
            name,
            arguments,
            return_type,
            is_variadic,
        })
    }

    fn extern_stmt(&mut self) -> Option<Statement> {
        let source = match self.current_token() {
            Some(TokenData::String(content)) => content,
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "extern header path",
                    found: self.full_current_token(),
                });
                return None;
            }
        };

        self.current += 1;

        match self.current_token_no_whitespace() {
            Some(TokenData::LeftBrace) => self.current += 1,
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "extern block body",
                    found: self.full_current_token(),
                });
                return None;
            }
        }

        let body = self.extern_body()?;

        Some(Statement::ExternBlock { source, body })
    }

    fn extern_body(&mut self) -> Option<Vec<Statement>> {
        let mut statements = vec![];

        // handle starting newlines or semicolons
        'consume_separator: loop {
            match self.current_token_no_whitespace() {
                Some(TokenData::RightBrace) => {
                    self.current += 1;
                    return Some(statements);
                }
                None => self.errors.push(ParserError::Expected {
                    rule: "function declaration or `}` in extern block",
                    found: None,
                }),
                _ => break 'consume_separator,
            }
        }

        loop {
            match self.current_token() {
                Some(TokenData::Def) => self.current += 1,
                _ => {
                    self.errors.push(ParserError::Expected {
                        rule: "def statement in extern block",
                        found: self.full_current_token(),
                    });
                    return None;
                }
            }

            statements.push(self.extern_def_stmt()?);

            let mut first = true;
            'consume_separator: loop {
                match self.current_token() {
                    Some(TokenData::NewLine) => self.current += 1,
                    Some(TokenData::RightBrace) => {
                        self.current += 1;
                        return Some(statements);
                    }
                    None => return Some(statements),
                    _ => {
                        if first {
                            self.errors.push(ParserError::Expected {
                                rule: "new line",
                                found: self.full_current_token(),
                            })
                        } else {
                            break 'consume_separator;
                        }
                    }
                }
                first = false;
            }
        }
    }

    fn function_head(&mut self) -> Option<(String, Vec<(String, Type)>, Type, bool)> {
        if let Some(TokenData::Identifier(name)) = self.current_token() {
            self.current += 1;
            if let Some(TokenData::LeftParen) = self.current_token() {
                self.current += 1;
                let mut args = vec![];
                let mut is_variadic = false;
                loop {
                    match self.current_token() {
                        Some(TokenData::RightParen) => {
                            self.current += 1;
                            break;
                        }
                        Some(TokenData::Identifier(arg_name)) => {
                            self.current += 1;
                            match self.current_token() {
                                Some(TokenData::Colon) => {
                                    self.current += 1;
                                    let arg_type = self.type_()?;

                                    args.push((arg_name, arg_type));

                                    match self.current_token() {
                                        Some(TokenData::Comma) => {
                                            self.current += 1;
                                        }
                                        Some(TokenData::RightParen) => {
                                            self.current += 1;
                                            break;
                                        }
                                        _ => {
                                            self.errors.push(ParserError::Expected {
                                                rule: "',' or ')' in function declaration",
                                                found: self.full_current_token(),
                                            });
                                            return None;
                                        }
                                    }
                                }
                                _ => {
                                    self.errors.push(ParserError::Expected {
                                        rule: "':' in function declaration",
                                        found: self.full_current_token(),
                                    });
                                    return None;
                                }
                            }
                        }
                        Some(TokenData::Ellipsis) => {
                            self.current += 1;
                            is_variadic = true;
                            match self.current_token() {
                                Some(TokenData::RightParen) => {
                                    self.current += 1;
                                    break;
                                }
                                _ => {
                                    self.errors.push(ParserError::Expected {
                                        rule: "')'",
                                        found: self.full_current_token(),
                                    });
                                    return None;
                                }
                            }
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "')' (end of arguments), identifier (argument name) or '...' (variadic arguments)",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }

                match self.current_token_no_whitespace() {
                    Some(TokenData::Arrow) => {
                        self.current += 1;
                        let return_type = self.type_()?;

                        return Some((name, args, return_type, is_variadic));
                    }
                    _ => return Some((name, args, Type::Void, is_variadic)),
                }
            } else {
                self.errors.push(ParserError::Expected {
                    rule: "'('",
                    found: self.full_current_token(),
                })
            }
        } else {
            self.errors.push(ParserError::Expected {
                rule: "identifier",
                found: self.full_current_token(),
            })
        }

        None
    }

    fn function_body(&mut self) -> Option<Vec<Statement>> {
        let mut statements = vec![];

        // handle starting newlines or semicolons
        'consume_separator: loop {
            match self.current_token_no_whitespace() {
                Some(TokenData::Semicolon) => self.current += 1,
                Some(TokenData::RightBrace) => {
                    self.current += 1;
                    return Some(statements);
                }
                None => return Some(statements),
                _ => break 'consume_separator,
            }
        }

        while self.current_token_no_whitespace().is_some() {
            statements.push(self.statement()?);

            let mut first = true;
            'consume_separator: loop {
                match self.current_token() {
                    Some(TokenData::NewLine) => self.current += 1,
                    Some(TokenData::Semicolon) => self.current += 1,
                    Some(TokenData::RightBrace) => {
                        self.current += 1;
                        return Some(statements);
                    }
                    None => return Some(statements),
                    _ => {
                        if first {
                            self.errors.push(ParserError::Expected {
                                rule: "new line or `;` (function)",
                                found: self.full_current_token(),
                            })
                        } else {
                            break 'consume_separator;
                        }
                    }
                }
                first = false;
            }
        }

        Some(statements)
    }

    fn struct_stmt(&mut self) -> Option<Statement> {
        let name = match self.current_token() {
            Some(TokenData::Identifier(name)) => name,
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "struct name",
                    found: self.full_current_token(),
                });
                return None;
            }
        };

        self.current += 1;

        match self.current_token_no_whitespace() {
            Some(TokenData::LeftBrace) => self.current += 1,
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "`{` after struct name",
                    found: self.full_current_token(),
                });
                return None;
            }
        }

        let mut members = HashMap::new();

        loop {
            match self.current_token_no_whitespace() {
                Some(TokenData::Identifier(name)) => {
                    self.current += 1;
                    match self.current_token() {
                        Some(TokenData::Colon) => {
                            self.current += 1;
                            members.insert(name, self.type_()?);
                            match self.current_token_no_whitespace() {
                                Some(TokenData::RightBrace) => {
                                    self.current += 1;
                                    break;
                                }
                                Some(TokenData::Comma) => self.current += 1,
                                _ => {
                                    self.errors.push(ParserError::Expected {
                                        rule: "`,` or `}` in struct declaration",
                                        found: self.full_current_token(),
                                    });
                                    return None;
                                }
                            }
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "`:` after struct member name",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }
                Some(TokenData::RightBrace) => {
                    self.current += 1;
                    break;
                }
                _ => {
                    self.errors.push(ParserError::Expected {
                        rule: "identifier or `}` in struct declaration",
                        found: self.full_current_token(),
                    });
                    return None;
                }
            }
        }
        Some(Statement::StructDeclaration { name, members })
    }

    fn if_stmt(&mut self) -> Option<Statement> {
        let mut conditions_and_bodies = vec![];

        self.no_struct_init = true;
        let condition = self.expression()?;
        self.no_struct_init = false;

        let body = match self.current_token_no_whitespace() {
            Some(TokenData::LeftBrace) => {
                self.current += 1;
                self.function_body()?
            }
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "if block body",
                    found: self.full_current_token(),
                });
                return None;
            }
        };

        conditions_and_bodies.push((condition, body));

        let mut else_body: Option<Vec<Statement>> = None;
        loop {
            match self.current_token_no_whitespace() {
                Some(TokenData::Elif) => {
                    self.current += 1;

                    self.no_struct_init = true;
                    let condition = self.expression()?;
                    self.no_struct_init = false;

                    match self.current_token_no_whitespace() {
                        Some(TokenData::LeftBrace) => {
                            self.current += 1;
                            let body = self.function_body()?;
                            conditions_and_bodies.push((condition, body));
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "elif block body",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }
                Some(TokenData::Else) => {
                    self.current += 1;
                    match self.current_token_no_whitespace() {
                        Some(TokenData::LeftBrace) => {
                            self.current += 1;
                            else_body = Some(self.function_body()?);
                            break;
                        }
                        _ => {
                            self.errors.push(ParserError::Expected {
                                rule: "else block body",
                                found: self.full_current_token(),
                            });
                            return None;
                        }
                    }
                }
                _ => break,
            }
        }
        Some(Statement::If {
            conditions_and_bodies,
            else_body,
        })
    }

    fn while_stmt(&mut self) -> Option<Statement> {
        self.no_struct_init = true;
        let condition = self.expression()?;
        self.no_struct_init = false;

        let body = match self.current_token_no_whitespace() {
            Some(TokenData::LeftBrace) => {
                self.current += 1;
                self.function_body()?
            }
            _ => {
                self.errors.push(ParserError::Expected {
                    rule: "if block body",
                    found: self.full_current_token(),
                });
                return None;
            }
        };

        Some(Statement::While { condition, body })
    }

    fn return_stmt(&mut self) -> Option<Statement> {
        Some(match self.current_token() {
            Some(TokenData::NewLine | TokenData::Semicolon) | None => Statement::Return(None),
            _ => Statement::Return(Some(self.expression()?)),
        })
    }

    fn import_stmt(&mut self) -> Option<Statement> {
        Some(match self.current_token() {
            Some(TokenData::Dot) => {
                self.current += 1;
                match self.current_token() {
                    Some(TokenData::Identifier(name)) => {
                        self.current += 1;
                        Statement::Import(Import::Relative(name))
                    }
                    _ => {
                        self.errors.push(ParserError::Expected {
                            rule: "identifier",
                            found: self.full_current_token(),
                        });
                        return None;
                    }
                }
            }
            _ => Statement::Import(Import::Absolute(self.resource_path()?)),
        })
    }

    fn resource_path(&mut self) -> Option<Vec<String>> {
        let mut path = vec![];

        loop {
            match self.current_token() {
                Some(TokenData::Identifier(name)) => {
                    path.push(name);
                    self.current += 1;
                    match self.current_token() {
                        Some(TokenData::Dot) => self.current += 1,
                        _ => return Some(path),
                    }
                }
                _ => {
                    self.errors.push(ParserError::Expected {
                        rule: "identifier in resource path",
                        found: self.full_current_token(),
                    });
                    return None;
                }
            }
        }
    }
}

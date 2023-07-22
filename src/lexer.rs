use std::{collections::HashMap, fmt::Display, iter::Peekable};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
    // structure
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,

    // operators
    Colon,
    Comma,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Ref,
    Arrow,

    // assignments
    Assign,

    // comparaisons
    Equal,
    NotEqual,
    Greater,
    GreatorOrEqual,
    Lesser,
    LesserOrEqual,

    // literals
    Identifier(String),
    String(String),
    Integer(String, Option<(bool, Option<usize>)>),
    Float(String, Option<usize>),

    // keywords
    Not,
    And,
    Or,
    If,
    Else,
    Elif,
    For,
    While,
    Def,
    Extern,
    Import,
    Struct,
    Let,
    Return,
    Break,
    False,
    True,
    Null,
    Ellipsis,

    // specials
    NewLine,
    Unknown(String),
}

impl Display for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenData::*;
        let mut done = true;

        write!(
            f,
            "{}",
            match self {
                LeftParen => "(",
                RightParen => ")",
                LeftBrace => "{",
                RightBrace => "}",
                LeftBracket => "[",
                RightBracket => "]",
                Semicolon => ";",
                Colon => ":",
                Comma => ",",
                Dot => ".",
                Plus => "+",
                Minus => "-",
                Star => "*",
                Slash => "/",
                Ref => "&",
                Assign => "=",
                Equal => "==",
                NotEqual => "!=",
                Greater => ">",
                GreatorOrEqual => ">=",
                Lesser => "<",
                LesserOrEqual => "<=",
                Not => "not",
                And => "and",
                Or => "or",
                If => "if",
                Else => "else",
                Elif => "elif",
                For => "for",
                While => "while",
                Def => "def",
                Extern => "extern",
                Struct => "struct",
                Let => "let",
                Return => "return",
                Break => "break",
                False => "false",
                True => "true",
                Null => "null",
                Ellipsis => "...",
                NewLine => "\n\\n",
                _ => {
                    done = false;
                    ""
                }
            }
        )?;

        if !done {
            Ok(write!(f, "{:?}", self)?)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub data: TokenData,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum LexingError {
    MisplacedCharacter(char),
    UnTerminatedString((usize, usize)),
    UnMatchedRightParen((usize, usize))
}

#[derive(Debug, Clone)]
pub struct LexingResult {
    pub tokens: Vec<Token>,
    pub errors: HashMap<(usize, usize), LexingError>,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub chars: Peekable<std::str::Chars<'a>>,
    pub errors: HashMap<(usize, usize), LexingError>,

    current_line: usize,
    current_column: usize,
    no_new_line_depth: usize,
    newline_escape_mode: bool
}

impl<'a> Lexer<'a> {
    fn token(&self, data: TokenData) -> Token {
        Token {
            data,
            line: self.current_line,
            column: self.current_column,
        }
    }

    fn new_line(&mut self) {
        self.current_column = 0;
        self.current_line += 1;
    }

    fn syntax_error(&mut self, error: LexingError) {
        self.errors
            .insert((self.current_line, self.current_column), error);
    }

    fn string(&mut self) -> Result<Token, LexingError> {
        let mut content = String::new();
        let start_position = (self.current_line, self.current_column);

        loop {
            self.current_column += 1;
            match self.chars.next() {
                None => return Err(LexingError::UnTerminatedString(start_position)),
                Some('"') => {
                    return Ok(Token {
                        data: TokenData::String(content),
                        line: start_position.0,
                        column: start_position.1,
                    })
                }
                Some('\n') => {
                    self.new_line();
                    content.push('\n');
                }
                Some(ch) => content.push(ch),
            };
        }
    }

    fn identifier(&mut self, start: char) -> Token {
        let mut content = start.to_string();
        let start_position = (self.current_line, self.current_column);

        loop {
            match self.chars.peek() {
                Some(ch) if ch.is_alphanumeric() || *ch == '_' => {
                    self.current_column += 1;
                    content.push(
                        self.chars
                            .next()
                            .expect("tokenized string should not terminate"),
                    );
                }
                _ => {
                    return Token {
                        data: match content.as_str() {
                            "not" => TokenData::Not,
                            "and" => TokenData::And,
                            "or" => TokenData::Or,
                            "if" => TokenData::If,
                            "else" => TokenData::Else,
                            "elif" => TokenData::Elif,
                            "for" => TokenData::For,
                            "while" => TokenData::While,
                            "def" => TokenData::Def,
                            "extern" => TokenData::Extern,
                            "import" => TokenData::Import,
                            "struct" => TokenData::Struct,
                            "let" => TokenData::Let,
                            "return" => TokenData::Return,
                            "break" => TokenData::Break,
                            "false" => TokenData::False,
                            "true" => TokenData::True,
                            "null" => TokenData::Null,
                            _ => TokenData::Identifier(content),
                        },
                        line: start_position.0,
                        column: start_position.1,
                    }
                }
            };
        }
    }

    fn digit_sequence(&mut self, start: &mut String) {
        loop {
            match self.chars.peek() {
                Some( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ) => {
                    self.current_column += 1;
                    start.push(
                        self.chars
                            .next()
                            .expect("peek was Some"),
                    );
                }
                _ => break
            };
        }
    }

    fn number(&mut self, start: char) -> Token {
        let mut content = start.to_string();

        let start_position = (self.current_line, self.current_column);

        self.digit_sequence(&mut content);

        let signed;
        match self.chars.peek() {
            Some('.') => {
                content.push(self.chars.next().expect("peek was Some"));

                self.digit_sequence(&mut content);

                return match self.chars.peek() {
                    Some('f') => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ) => {
                                let mut size = String::with_capacity(3); // most of the time float type size should be 32, 64 or 128
                                self.digit_sequence(&mut size);
                                let size: usize =  size.parse().expect("float type size should only be ASCII digits");

                                Token {
                                    data: TokenData::Float(content, Some(size)),
                                    line: start_position.0,
                                    column: start_position.1
                                }
                            },
                            _ => Token {
                                data: TokenData::Float(content, None),
                                line: start_position.0,
                                column: start_position.1
                            }
                        }
                    },
                    _ => Token {
                        data: TokenData::Float(content, None),
                        line: start_position.0,
                        column: start_position.1
                    }
                }
            }
            Some('u') => signed = false,
            Some('i') => signed = true,
            _ => return Token {
                data: TokenData::Integer(content, None),
                line: start_position.0,
                column: start_position.1
            }
        }

        self.chars.next();
        match self.chars.peek() {
            Some( '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ) => {
                let mut size = String::with_capacity(3);
                self.digit_sequence(&mut size);

                let size: usize = size.parse().expect("int type size should only be ASCII digits");

                Token {
                    data: TokenData::Integer(content, Some((signed, Some(size)))),
                    line: start_position.0,
                    column: start_position.1
                }
            },
            _ => Token {
                data: TokenData::Integer(content, Some((signed, None))),
                line: start_position.0,
                column: start_position.1
            }
        }
    }

    pub fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
            current_line: 1,
            current_column: 0,
            no_new_line_depth: 0,
            newline_escape_mode: false,
            errors: HashMap::new(),
        }
    }

    pub fn lex(mut self) -> LexingResult {
        let mut tokens = vec![];
        for tok in self.by_ref() {
            tokens.push(tok)
        }

        LexingResult {
            tokens,
            errors: self.errors,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        use TokenData::*;

        macro_rules! two_char_token {
            ($second_char:expr, $short_tok:expr, $long_tok:expr) => {
                match self.chars.peek() {
                    Some($second_char) => {
                        self.current_column += 1;
                        self.chars.next();
                        let token = $long_tok;

                        token
                    }
                    _ => $short_tok,
                }
            };
        }

        self.current_column += 1;

        match self.chars.next() {
            Option::Some(ch) => Option::Some(match ch {
                '(' => {
                    self.no_new_line_depth += 1;
                    self.token(LeftParen)
                },
                ')' => {
                    if self.no_new_line_depth != 0 {
                        self.no_new_line_depth -= 1;
                    } else {
                        self.syntax_error(LexingError::UnMatchedRightParen((self.current_line, self.current_column)));
                    }
                    self.token(RightParen)
                },
                '{' => self.token(LeftBrace),
                '}' => self.token(RightBrace),
                '[' => self.token(LeftBracket),
                ']' => self.token(RightBracket),
                ';' => {
                    self.newline_escape_mode = false;
                    self.token(Semicolon)
                },

                ':' => self.token(Colon),
                ',' => self.token(Comma),
                '.' => match self.chars.peek() {
                    Some('.') => {
                        self.chars.next();

                        match self.chars.next() {
                            Some('.') => self.token(Ellipsis),
                            Some(other) => {
                                self.syntax_error(LexingError::MisplacedCharacter(other));
                                self.token(Unknown(other.to_string()))
                            }
                            None => return None,
                        }
                    }
                    _ => self.token(Dot),
                },
                '+' => self.token(Plus),
                '-' => two_char_token!('>', self.token(Minus), self.token(Arrow)),
                '*' => self.token(Star),
                '/' => match self.chars.peek() {
                    Some('/') => {
                        loop {
                            if matches!(self.chars.next(), Option::Some('\n') | None) {
                                self.new_line();
                                break;
                            }
                        }
                        self.next()?
                    }
                    _ => self.token(Slash),
                },
                '&' => self.token(Ref),
                '=' => two_char_token!('=', self.token(Assign), self.token(Equal)),
                '!' => two_char_token!(
                    '=',
                    {
                        self.syntax_error(LexingError::MisplacedCharacter('!'));
                        self.token(Unknown("!".to_string()))
                        // self.next()?
                    },
                    self.token(NotEqual)
                ),
                '>' => two_char_token!('=', self.token(Greater), self.token(GreatorOrEqual)),
                '<' => two_char_token!('=', self.token(Lesser), self.token(LesserOrEqual)),
                '"' => match self.string() {
                    Ok(token) => token,
                    Err(err) => {
                        self.syntax_error(err);
                        self.next()?
                    }
                },
                '\n' => {
                    self.new_line();
                    if self.no_new_line_depth == 0 && !self.newline_escape_mode {
                        self.token(NewLine)
                    } else {
                        self.next()?
                    }               
                },
                '\\' => {
                    match self.next() {
                        Some(Token { data: NewLine, .. }) => {
                            self.newline_escape_mode = true;
                            self.next()?
                        },
                        tok => {
                            self.syntax_error(LexingError::MisplacedCharacter('\\'));
                            tok?
                        }

                    }
                },
                letter if letter.is_alphabetic() || letter == '_' => self.identifier(letter),
                letter if letter.is_numeric() => self.number(letter),
                ' ' | '\t' => self.next()?,
                other => {
                    self.syntax_error(LexingError::MisplacedCharacter(other));
                    self.token(Unknown(other.to_string()))
                    // self.next()?
                }
            }),
            None => None,
        }
    }
}

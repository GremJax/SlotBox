use crate::ValueKind;
use crate::Value;

#[derive(Debug, Clone)]
pub enum Operator {
    Add, Sub, Mul, Div, Mod,
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
    Inc, Dec, 
    BWAnd, BWOr, BWXor, BWNot, BWShiftL, BWShiftR,
    Equal, NEqual, LT, GT, LTE, GTE, 
    Not, And, Or,
    Range, RangeLT, At, Hash,
    Dot, Colon, DColon, Question, DQuestion,
    Assign, Attach, Detach, Arrow,
    IsShape, NIsShape
}

impl Operator {
    pub fn kind(&self) -> ValueKind {
        match self {
            Operator::Add | Operator::Mul | Operator::Div | Operator::Sub | Operator::Mod |
            Operator::BWAnd | Operator::BWOr | Operator::BWXor | Operator::BWNot | Operator::BWShiftL | Operator::BWShiftR |
            Operator::Inc | Operator::Dec
                => ValueKind::Int32,
                
            Operator::Equal | Operator::NEqual | Operator::And | Operator::Or | Operator::Not |
            Operator::IsShape | Operator::NIsShape 
                => ValueKind::Bool,
                
            Operator::Range | Operator::RangeLT
                => ValueKind::Array(Box::new(ValueKind::Int32)),

            _ => ValueKind::None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Using,
    Shape, Let,
    Static, Func, Sealed,
    If, Else, Switch,
    While, For, In,
    Break, Return, Continue,
    Print,
}

#[derive(Debug, Clone)]
pub enum Token{
    Identifier(String),
    Number(f64),
    Bool(bool),
    String(String),
    Operator(Operator),
    Keyword(Keyword),
    Type(ValueKind),
    LeftParen,
    RightParen,
    Comma,
    NewLine,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    EOF,
}

static WHITESPACE: &[char] = &[' ', '\t', '\n', ';'];

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ws if WHITESPACE.contains(&ws) => { chars.next(); },
            '\n' => {
                //tokens.push(Token::NewLine);
                chars.next();
            },
            '(' => {
                tokens.push(Token::LeftParen);
                chars.next();
            },
            ')' => {
                tokens.push(Token::RightParen);
                chars.next();
            },
            '{' => {
                tokens.push(Token::LeftBrace);
                chars.next();
            },
            '}' => {
                tokens.push(Token::RightBrace);
                chars.next();
            },
            '[' => {
                tokens.push(Token::LeftBracket);
                chars.next();
            },
            ']' => {
                tokens.push(Token::RightBracket);
                chars.next();
            },
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            },
            '0'..='9' => {
                let mut number = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_digit(10) || ch == '.' {
                        number.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(number.parse().unwrap()));
            },
            '"' => {
                chars.next(); // skip opening quote
                let mut string = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '"' {
                        break;
                    } else {
                        string.push(ch);
                        chars.next();
                    }
                }
                chars.next(); // skip closing quote
                tokens.push(Token::String(string));
            },
            _ if ch.is_alphabetic() || ch == '_' => {
                let mut identifier = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        identifier.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }

                tokens.push(match identifier.as_str() {
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),

                    "using" => Token::Keyword(Keyword::Using),
                    "shape" => Token::Keyword(Keyword::Shape),
                    "let" => Token::Keyword(Keyword::Let),
                    "static" => Token::Keyword(Keyword::Static),
                    "func" => Token::Keyword(Keyword::Func),
                    "sealed" => Token::Keyword(Keyword::Sealed),
                    "if" => Token::Keyword(Keyword::If),
                    "else" => Token::Keyword(Keyword::Else),
                    "switch" => Token::Keyword(Keyword::Switch),
                    "while" => Token::Keyword(Keyword::While),
                    "for" => Token::Keyword(Keyword::For),
                    "in" => Token::Keyword(Keyword::In),
                    "break" => Token::Keyword(Keyword::Break),
                    "return" => Token::Keyword(Keyword::Return),
                    "continue" => Token::Keyword(Keyword::Continue),
                    "print" => Token::Keyword(Keyword::Print),

                    "int32" => Token::Type(ValueKind::Int32),
                    "bool" => Token::Type(ValueKind::Bool),
                    "string" => Token::Type(ValueKind::String),
                    "void" => Token::Type(ValueKind::None),

                    _ => Token::Identifier(identifier),
                });

            },
            _ => {
                let mut operator = String::new();
                while let Some(&ch) = chars.peek() {
                    if !WHITESPACE.contains(&ch) && !ch.is_alphanumeric() && ch != '_' {
                        operator.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                
                if operator == "//" {
                    // Skip single-line comment
                    while let Some(ch) = chars.next() {
                        if ch == '\n' {
                            break;
                        }
                    }
                }
                
                if operator == "/*" {
                    // Skip multi-line comment
                    while let Some(ch) = chars.next() {
                        if ch == '*' && chars.peek() == Some(&'/') {
                            chars.next();
                            break;
                        }
                    }
                }

                tokens.push( Token::Operator(match operator.as_str() {
                    "+" => Operator::Add,
                    "-" => Operator::Sub,
                    "/" => Operator::Div,
                    "*" => Operator::Mul,
                    "%" => Operator::Mod,

                    "+=" => Operator::AddAssign,
                    "-=" => Operator::SubAssign,
                    "/=" => Operator::DivAssign,
                    "*=" => Operator::MulAssign,
                    "%=" => Operator::ModAssign,

                    "++" => Operator::Inc,
                    "--" => Operator::Dec,

                    "=" => Operator::Assign,
                    ":=" => Operator::Attach,
                    "=:" => Operator::Detach,
                    "->" => Operator::Arrow,

                    "==" => Operator::Equal,
                    "!=" => Operator::NEqual,
                    "<" => Operator::LT,
                    ">" => Operator::GT,
                    "<=" => Operator::LTE,
                    ">=" => Operator::GTE,

                    "&&" => Operator::And,
                    "||" => Operator::Or,
                    "!" => Operator::Not,

                    "=~" => Operator::IsShape,
                    "!~" => Operator::NIsShape,

                    "&" => Operator::BWAnd,
                    "|" => Operator::BWOr,
                    "^" => Operator::BWXor,
                    "~" => Operator::BWNot,
                    "<<" => Operator::BWShiftL,
                    ">>" => Operator::BWShiftR,

                    "..." => Operator::Range,
                    "..<" => Operator::RangeLT,
                    "@" => Operator::At,
                    "#" => Operator::Hash,

                    "." => Operator::Dot,
                    ":" => Operator::Colon,
                    "::" => Operator::DColon,
                    "?" => Operator::Question,
                    "??" => Operator::DQuestion,
                    
                    _ => panic!("Unknown operator: {}", operator)
                }))
            }
        }
    }

    tokens.push(Token::EOF);
    tokens
}
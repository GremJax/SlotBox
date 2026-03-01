use std::fmt;

use crate::ValueKind;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Operator {
    Add, Sub, Mul, Div, Mod,
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign, 
    AndAssign, OrAssign, XorAssign, ShiftLAssign, ShiftRAssign,
    Inc, Dec, Len,
    BWAnd, BWOr, BWXor, BWNot, BWShiftL, BWShiftR,
    Equal, NEqual, LT, GT, LTE, GTE, 
    Not, And, Or,
    Range, RangeLT, 
    At, Hash, Dollar,
    Dot, Colon, DColon, Question, DQuestion,
    Assign, Attach, Detach, Arrow, FatArrow,
    IsShape, NIsShape
}

pub const UNARY_OPERATORS: &[Operator] = &[ Operator::BWNot, Operator::Not, Operator::Dec, Operator::Inc, Operator::Sub, Operator::Len ];
pub const BINARY_OPERATORS: &[Operator] = &[
    Operator::Add, Operator::Sub, Operator::Mul, Operator::Div, Operator::Mod,
    Operator::Equal, Operator::NEqual, Operator::GT, Operator::LT, Operator::GTE, Operator::LTE,
    Operator::BWAnd, Operator::BWOr, Operator::BWXor, Operator::BWShiftL, Operator::BWShiftR,
    Operator::And, Operator::Or, Operator::IsShape, Operator::NIsShape,
    Operator::Range, Operator::RangeLT,
];

impl Operator {
    pub fn kind(&self) -> ValueKind {
        match self {
            Operator::Add | Operator::Mul | Operator::Div | Operator::Sub | Operator::Mod |
            Operator::BWAnd | Operator::BWOr | Operator::BWXor | Operator::BWNot | Operator::BWShiftL | Operator::BWShiftR |
            Operator::Inc | Operator::Dec | Operator::Len
                => ValueKind::Int32,
                
            Operator::Equal | Operator::NEqual | Operator::And | Operator::Or | Operator::Not |
            Operator::LT | Operator::GT | Operator::LTE | Operator::GTE | 
            Operator::IsShape | Operator::NIsShape 
                => ValueKind::Bool,
                
            Operator::Range | Operator::RangeLT
                => ValueKind::Array(Box::new(ValueKind::Int32)),

            _ => ValueKind::None
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Keyword {
    Using,
    Shape, Let,
    Static, Seal, Locked, Const,
    Before, After, Next,
    Func, PSelf, 
    If, Else, Switch,
    While, Loop, For, In,
    Break, Return, Continue, Throw,
    Print,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}]", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind{
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

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

static WHITESPACE: &[char] = &[' ', '\n', '\t', ';'];

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    
    let mut line = 1;
    let mut column = 1;

    while let Some(&ch) = chars.peek() {
        let span = Span{line, column};

        match ch {
            '\n' => {
                chars.next();
                line += 1;
                column = 0;
            },
            ws if WHITESPACE.contains(&ws) => { chars.next(); },
            '(' => {
                tokens.push(Token{span, kind:TokenKind::LeftParen});
                chars.next();
                column += 1;
            },
            ')' => {
                tokens.push(Token{span, kind:TokenKind::RightParen});
                chars.next();
                column += 1;
            },
            '{' => {
                tokens.push(Token{span, kind:TokenKind::LeftBrace});
                chars.next();
                column += 1;
            },
            '}' => {
                tokens.push(Token{span, kind:TokenKind::RightBrace});
                chars.next();
                column += 1;
            },
            '[' => {
                tokens.push(Token{span, kind:TokenKind::LeftBracket});
                chars.next();
                column += 1;
            },
            ']' => {
                tokens.push(Token{span, kind:TokenKind::RightBracket});
                chars.next();
                column += 1;
            },
            ',' => {
                tokens.push(Token{span, kind:TokenKind::Comma});
                chars.next();
                column += 1;
            },
            '0'..='9' => {
                let mut number = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_digit(10) || ch == '.' {
                        number.push(ch);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token{span, kind:TokenKind::Number(number.parse().unwrap())});
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
                        column += 1;
                    }
                }
                chars.next(); // skip closing quote
                tokens.push(Token{span, kind:TokenKind::String(string)});
            },
            _ if ch.is_alphabetic() || ch == '_' => {
                let mut identifier = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        identifier.push(ch);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }

                tokens.push(Token{span, kind: match identifier.as_str() {
                    "true" => TokenKind::Bool(true),
                    "false" => TokenKind::Bool(false),

                    "using" => TokenKind::Keyword(Keyword::Using),
                    "shape" => TokenKind::Keyword(Keyword::Shape),
                    "let" => TokenKind::Keyword(Keyword::Let),
                    "static" => TokenKind::Keyword(Keyword::Static),
                    "func" => TokenKind::Keyword(Keyword::Func),
                    "self" => TokenKind::Keyword(Keyword::PSelf),
                    "seal" => TokenKind::Keyword(Keyword::Seal),
                    "locked" => TokenKind::Keyword(Keyword::Locked),
                    "const" => TokenKind::Keyword(Keyword::Const),
                    "before" => TokenKind::Keyword(Keyword::Before),
                    "after" => TokenKind::Keyword(Keyword::After),
                    "if" => TokenKind::Keyword(Keyword::If),
                    "else" => TokenKind::Keyword(Keyword::Else),
                    "switch" => TokenKind::Keyword(Keyword::Switch),
                    "while" => TokenKind::Keyword(Keyword::While),
                    "loop" => TokenKind::Keyword(Keyword::Loop),
                    "for" => TokenKind::Keyword(Keyword::For),
                    "in" => TokenKind::Keyword(Keyword::In),
                    "break" => TokenKind::Keyword(Keyword::Break),
                    "return" => TokenKind::Keyword(Keyword::Return),
                    "continue" => TokenKind::Keyword(Keyword::Continue),
                    "throw" => TokenKind::Keyword(Keyword::Throw),
                    "print" => TokenKind::Keyword(Keyword::Print),

                    "len" => TokenKind::Operator(Operator::Len),
                    "and" => TokenKind::Operator(Operator::And),
                    "or" => TokenKind::Operator(Operator::Or),

                    "number" => TokenKind::Type(ValueKind::Number),
                    "int32" => TokenKind::Type(ValueKind::Int32),
                    "byte" => TokenKind::Type(ValueKind::UInt8),
                    "bool" => TokenKind::Type(ValueKind::Bool),
                    "string" => TokenKind::Type(ValueKind::String),
                    "void" => TokenKind::Type(ValueKind::None),

                    _ => TokenKind::Identifier(identifier),
                }});
            },
            _ => {
                let mut operator = String::new();
                let mut comment_line = false;

                while let Some(&ch) = chars.peek() {

                    if !WHITESPACE.contains(&ch) && !ch.is_alphanumeric() && ch != '_' {
                        chars.next();
                        column += 1;

                        // Single line comment
                        if ch == '/' && chars.peek() == Some(&'/') {
                            while let Some(ch) = chars.next() {
                                column += 1;
                                if ch == '\n' {
                                    line += 1;
                                    column = 0;
                                    break;
                                }
                            }
                            comment_line = true;
                            break;
                        }

                        // Multi line comment
                        if ch == '/' && chars.peek() == Some(&'*') {
                            while let Some(ch) = chars.next() {
                                column += 1;
                                if ch == '\n' {
                                    line += 1;
                                    column = 0;
                                }
                                if ch == '*' && chars.peek() == Some(&'/') {
                                    chars.next();
                                    break;
                                }
                            }
                            break;
                        }

                        operator.push(ch);
                    } else {
                        break;
                    }
                }

                if comment_line { continue; }
                
                tokens.push(Token{span:span.clone(), kind:TokenKind::Operator(match operator.as_str() {
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

                    "&" => Operator::BWAnd,
                    "|" => Operator::BWOr,
                    "^" => Operator::BWXor,
                    "~" => Operator::BWNot,
                    "<<" => Operator::BWShiftL,
                    ">>" => Operator::BWShiftR,
                     
                    "&=" => Operator::AndAssign,
                    "|=" => Operator::OrAssign,
                    "^=" => Operator::XorAssign,
                    "<<=" => Operator::ShiftLAssign,
                    ">>=" => Operator::ShiftRAssign,

                    "++" => Operator::Inc,
                    "--" => Operator::Dec,

                    "=" => Operator::Assign,
                    ":=" => Operator::Attach,
                    "=:" => Operator::Detach,
                    "->" => Operator::Arrow,
                    "=>" => Operator::FatArrow,

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

                    "..." => Operator::Range,
                    "..<" => Operator::RangeLT,
                    "@" => Operator::At,
                    "#" => Operator::Hash,
                    "$" => Operator::Dollar,

                    "." => Operator::Dot,
                    ":" => Operator::Colon,
                    "::" => Operator::DColon,
                    "?" => Operator::Question,
                    "??" => Operator::DQuestion,
                    
                    _ => panic!("{}: Unknown operator: {}", span, operator)
                })});
            }
        }
    }

    tokens.push(Token{span:Span{line, column}, kind:TokenKind::EOF});
    tokens
}
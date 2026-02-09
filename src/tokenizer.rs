use crate::ValueKind;

#[derive(Debug, Clone)]
pub(crate) enum Token{
    Identifier(String),
    Number(f64),
    Bool(bool),
    String(String),
    Operator(String),
    Keyword(String),
    Type(ValueKind),
    LeftParen,
    RightParen,
    Comma,
    NewLine,
    LeftBrace,
    RightBrace,
    EOF,
}

static KEYWORDS: &[&str] = &["shape", "object", "print", "static", "let", "fn", "if", "else", "while", "return"]; 
static TYPES: &[&str] = &["int32", "bool", "string", "void"]; 
static OPERATORS: &[&str] = &[":=", "=:", "+", "-", "=", "==", "!=", "->", "<", ">", "<=", ">=", "&&", "||", "!"]; 

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' => { chars.next(); },
            '\n' => {
                tokens.push(Token::NewLine);
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
                if(KEYWORDS.contains(&identifier.as_str())) {
                    tokens.push(Token::Keyword(identifier));

                } else if identifier == "true" || identifier == "false" {
                    tokens.push(Token::Bool(identifier == "true"));

                } else if TYPES.contains(&identifier.as_str()) {
                    tokens.push(Token::Type(match identifier.as_str() {
                        "int32" => ValueKind::Int32,
                        "bool" => ValueKind::Bool,
                        "string" => ValueKind::String,
                        "void" => ValueKind::None,
                        _ => unreachable!(),
                    }));

                } else {
                    tokens.push(Token::Identifier(identifier));
                }
            },
            _ => {
                let mut operator = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        operator.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if OPERATORS.contains(&operator.as_str()) {
                    tokens.push(Token::Operator(operator));
                } else {
                    panic!("Unknown token: {}", operator);
                }
            }
        }
    }

    tokens.push(Token::EOF);
    tokens
}
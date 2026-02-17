use crate::tokenizer::{Keyword, Operator, Token};
use crate::PrimitiveValue;
use crate::Value;
use crate::ValueKind;

type Identifier = String;

#[derive(Debug, Clone)]
enum Presence {
    Guaranteed,
    Optional,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Value),
    Array(Vec<Expression>),
    Variable(Identifier),
    UnaryOp {
        operator: Operator,
        operand: Box<Expression>,
    },
    BinaryOp {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    MemberAccess  {
        target: Box<Expression>,
        member: Identifier,
    }
}


#[derive(Debug, Clone)]
pub enum ShapeExpression {
    Simple(Identifier),
    Parameter(Identifier), // T
    Primitive(ValueKind),
    Applied {
        base: Box<ShapeExpression>,
        args: Vec<ShapeExpression>,
    }
}

#[derive(Debug, Clone)]
pub struct Mapping {
    pub from_slot: Identifier,
    pub to_slot: Identifier,
}

#[derive(Debug, Clone)]
pub struct Azimuth {
    pub name: Identifier,
    pub value_type: ShapeExpression,
    pub is_static: bool,
    pub set_value: Option<Expression>
}

#[derive(Debug, Clone)]
pub enum Statement {
    Using { package: String },
    DeclareShape { 
        name: Identifier, 
        slot_ids: Vec<Azimuth>,
        mappings: Vec<Mapping>,
        generics: Vec<ShapeExpression>,
    },
    DeclareObject { name: Identifier }, 
    Attach { object: Expression, shape: ShapeExpression },
    Detach { object: Expression, shape: ShapeExpression },
    AddMapping { object: Expression, mapping: Mapping },
    AttachWithRemap { object: Expression, shape: ShapeExpression, mappings: Vec<Mapping> },
    Print { expr: Expression },
    If {
        condition: Expression,
        true_statement: Box<Statement>,
        else_statement: Box<Statement>,
    },
    While {
        condition: Expression,
        statement: Box<Statement>,
    },
    For {
        local: Expression,
        statement: Box<Statement>,
    },
    Assign { target: Expression, value: Expression, },

}

fn build_expression(tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>) -> Expression {
    if let Some(token) = tokens.next() {
        let mut expr = match token {
            Token::Number(num) => Expression::Literal((num as i32).into()),
            Token::Bool(b) => Expression::Literal(b.into()),
            Token::String(s) => Expression::Literal(s.into()),

            Token::Identifier(identifier) => Expression::Variable(identifier),

            // Array literal
            Token::LeftBracket => {
                let mut elements = Vec::new();

                while let Some(token) = tokens.peek() {
                    if matches!(token, Token::RightBracket) {
                        tokens.next();
                        break;
                    }

                    elements.push(build_expression(tokens));

                    if let Some(Token::Comma) = tokens.peek() {
                        tokens.next();
                    }
                }

                Expression::Array(elements)
            }

            other => panic!("Unexpected token in expression: {:?}", other),
        };

        // Check for member access
        while let Some(token) = tokens.peek() {
            if matches!(token, Token::Operator(Operator::Dot)) {
                tokens.next();
                if let Some(Token::Identifier(k)) = tokens.next() {
                    expr = Expression::MemberAccess{ target: Box::new(expr), member: k };
                }

            } else { break; }
        }
                
        expr

    } else {
        panic!("Unexpected end of tokens while building expression");
    }
}

fn parse_shape_expression(tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>, generic_params: &[Identifier],) -> ShapeExpression {
    let base = match tokens.next() {
        Some(Token::Identifier(id)) => {
            if generic_params.contains(&id) {
                ShapeExpression::Parameter(id)
            } else {
                ShapeExpression::Simple(id)
            }
        }
        Some(Token::Type(kind)) => ShapeExpression::Primitive(kind),
        other => panic!("Expected shape identifier, got {:?}", other),
    };

    if let Some(Token::Operator(Operator::LT)) = tokens.peek() {
        tokens.next(); // consume '<'
        let mut args = Vec::new();

        loop {
            let arg = parse_shape_expression(tokens, generic_params);
            args.push(arg);

            match tokens.next() {
                Some(Token::Comma) => continue,
                Some(Token::Operator(Operator::GT)) => break,
                other => panic!("Unexpected token in generic args: {:?}", other),
            }
        }

        ShapeExpression::Applied {
            base: Box::new(base),
            args,
        }
    } else {
        base
    }
}

pub fn parse(input: Vec<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut tokens = input.into_iter().peekable();

    while let Some(token) = tokens.peek() {
        match token {

            // Package import
            Token::Keyword(Keyword::Using) => {
                tokens.next(); // consume 'using' keyword
                let package = match tokens.next() {
                    Some(Token::String(name)) => name.clone(),
                    other => panic!("Expected package name after 'using', got {:?}", other),
                };

                statements.push(Statement::Using { package: package });
            },

            // Shape declaration
            Token::Keyword(Keyword::Shape) => {
                tokens.next(); // consume 'shape' keyword
                let shape_identifier = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected shape name after 'shape', got {:?}",
                        other
                    ),
                };

                let mut slot_ids = Vec::new();
                let mut mappings = Vec::new();

                let mut generics = Vec::new();
                let mut generic_names = Vec::new();

                // Establish generics
                if let Some(Token::Operator(Operator::LT)) = tokens.peek() {
                    tokens.next(); // consume '<'

                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Identifier(_) => {
                                let shape_expr = parse_shape_expression(&mut tokens, &mut generic_names);

                                match &shape_expr{
                                    ShapeExpression::Parameter(x) => generic_names.push(x.clone()),
                                    _ => {}
                                }

                                generics.push(shape_expr)
                            }
                            Token::Comma => { tokens.next(); }
                            Token::Operator(Operator::GT) => { 
                                tokens.next();
                                break
                            }
                            _ => panic!("Unexpected token in generic declaration: {:?}", token),
                        }
                    }
                }

                if let Some(Token::LeftBrace) = tokens.next() {

                    // Define slots
                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Identifier(slot_name) => {
                                let slot_name = slot_name.clone();
                                tokens.next();

                                // Type
                                let value_type = parse_shape_expression(&mut tokens, &mut generic_names);

                                // Static
                                let is_static = match tokens.peek() {
                                    Some(Token::Keyword(Keyword::Static)) => {
                                        tokens.next();
                                        true
                                    }
                                    _ => false
                                };

                                // Default
                                let set_value = match tokens.peek() {
                                    Some(Token::Operator(Operator::Assign)) => {
                                        tokens.next();
                                        Some(build_expression(&mut tokens))
                                    }
                                    _ => None
                                };

                                slot_ids.push(Azimuth { name: slot_name, value_type, is_static, set_value });
                            },
                            Token::RightBrace => {
                                tokens.next();
                                break;
                            },
                            _ => {
                                panic!("Unexpected token in shape declaration: {:?}", token);
                            }
                        }
                    }
                } else {
                    panic!("Expected '{{' after shape declaration");
                }

                statements.push(Statement::DeclareShape { name: shape_identifier, slot_ids, mappings, generics });
            },

            // Object declaration
            Token::Keyword(Keyword::Let) => {
                tokens.next(); // consume 'object' keyword
                let object_identifier = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected object name after 'object', got {:?}",
                        other
                    ),
                };

                statements.push(Statement::DeclareObject { name: object_identifier });
            },

            // print
            Token::Keyword(Keyword::Print) => {
                tokens.next(); // consume 'print' keyword
                statements.push(Statement::Print{ expr: build_expression(&mut tokens)});
            },

            // Object Identifier
            Token::Identifier(_) => {
                // Build member call
                let object = build_expression(&mut tokens);
                
                match tokens.peek() {
                    // Attach
                    Some(Token::Operator(Operator::Attach)) => {
                        tokens.next(); // consume operator
                        
                        let mut generics = Vec::new();
                        let shape = parse_shape_expression(&mut tokens, &mut generics);

                        // Check for mappings
                        if let Some(Token::LeftBrace) = tokens.peek() {
                            tokens.next(); // consume '{'
                            let mut mappings = Vec::new();

                            while let Some(token) = tokens.next() {
                                match token {
                                    // From slot mapping
                                    Token::Identifier(from_slot) => {

                                        // Expect '->' operator
                                        match tokens.next() {
                                            Some(Token::Operator(Operator::Arrow)) => {

                                                // Expect to slot identifier
                                                match tokens.next() {
                                                    Some(Token::Identifier(to_slot)) => mappings.push(Mapping { from_slot, to_slot }),
                                                    other => panic!("Expected slot name after -> in mapping, got {:?}", other),
                                                }

                                            },
                                            other => panic!("Expected '->' operator in mapping, got {:?}", other),
                                        }
                                    },
                                    Token::RightBrace => break,
                                    _ => panic!("Unexpected token in mappings: {:?}", token),
                                }
                            }

                            statements.push(Statement::AttachWithRemap { object, shape, mappings });
                        } else {
                            statements.push(Statement::Attach { object, shape });
                        }
                    }

                    //Detach
                    Some(Token::Operator(Operator::Detach)) => {
                        tokens.next(); // consume operator
                        
                        let shape = parse_shape_expression(&mut tokens, &Vec::new());

                        statements.push(Statement::Detach { object, shape });
                    }

                    // Assign
                    Some(Token::Operator(Operator::Assign)) => {
                        tokens.next(); // consume operator

                        let value = build_expression(&mut tokens);

                        statements.push(Statement::Assign{ target: object, value });
                    }

                    other => { panic!("Unexpected operator after object name: {:?}", other); }
                }
            },

            Token::EOF => break,

            _ => panic!("Unexpected token: {:?}", token),
        }
    }
    
    println!("\n Parsed Ast: \n{:?}", statements);
    statements
}
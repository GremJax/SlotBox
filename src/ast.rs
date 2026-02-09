use crate::tokenizer::Token;
use crate::PrimitiveValue;
use crate::Value;
use crate::ValueKind;

type Identifier = String;
enum Expression {
    Literal(Value),
    Variable(Identifier),
    BinaryOp {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    FunctionCall {
        name: Identifier,
        args: Vec<Expression>,
    },
}

struct Mapping {
    from_slot: Identifier,
    to_slot: Identifier,
}

struct SlotId {
    name: Identifier,
    value_type: ValueKind,
    is_static: bool,
}

enum Statement {
    DeclareShape { 
        name: Identifier, 
        slot_ids: Vec<SlotId>,
        mappings: Vec<Mapping>,
    },
    DeclareObject {
        name: Identifier
    },
    Attach { object: Identifier, shape: Identifier },
    Detach { object: Identifier, shape: Identifier },
    Print { object: Identifier },
    Assign {
        object: Identifier,
        slot: Identifier,
        value: Expression,
    }
}

fn parse(input: Vec<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut tokens = input.into_iter().peekable();

    while let Some(token) = tokens.peek() {
        match token {

            // Shape declaration
            Token::Keyword(k) if k == "shape" => {
                let shapeIdentifier = if let Some(Token::Identifier(name)) = tokens.next() { name }
                    else { panic!("Expected shape name after 'shape' keyword"); };

                let mut slot_ids = Vec::new();
                let mut mappings = Vec::new();

                if let Some(Token::LeftBrace) = tokens.next() {
                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Identifier(slot_name) => {
                                let slot_name = slot_name.clone();
                                tokens.next();

                                if let Some(Token::Type(value_type)) = tokens.next() {
                                    let value_type = value_type.clone();
                                    let is_static = if let Some(Token::Keyword(k)) = tokens.peek() {
                                        if k == "static" {
                                            tokens.next();
                                            true
                                        } else { false }
                                    } else { false };

                                    slot_ids.push(SlotId { name: slot_name, value_type, is_static });
                                } else {
                                    panic!("Expected type for slot '{}'", slot_name);
                                }
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

                statements.push(Statement::DeclareShape { name: shapeIdentifier, slot_ids, mappings });
            },

            // Object declaration
            Token::Keyword(k) if k == "object" => {
                let objectIdentifier = if let Some(Token::Identifier(name)) = tokens.next() { name }
                    else { panic!("Expected object name after 'object' keyword"); };

                statements.push(Statement::DeclareObject { name: objectIdentifier });
            },

            // attach
            Token::Keyword(k) if k == "attach" => {
                if let Some(Token::Identifier(object)) = tokens.next() {
                    if let Some(Token::Identifier(shape)) = tokens.next() {
                        statements.push(Statement::Attach { object, shape });
                    } else {
                        panic!("Expected shape name after object name in attach statement");
                    }
                } else {
                    panic!("Expected object name after 'attach' keyword");
                }
            },

            // detach
            Token::Keyword(k) if k == "detach" => {
                if let Some(Token::Identifier(object)) = tokens.next() {
                    if let Some(Token::Identifier(shape)) = tokens.next() {
                        statements.push(Statement::Detach { object, shape });
                    } else {
                        panic!("Expected shape name after object name in detach statement");
                    }
                } else {
                    panic!("Expected object name after 'detach' keyword");
                }
            },

            // print
            Token::Keyword(k) if k == "print" => {
                if let Some(Token::Identifier(object)) = tokens.next() {
                    statements.push(Statement::Print { object });
                } else {
                    panic!("Expected object name after 'print' keyword");
                }
            },

            // Assignment/call
            Token::Identifier(_) => {
                if let Some(Token::Identifier(object)) = tokens.next() {
                    if let Some(Token::Operator(op)) = tokens.peek() {
                        
                        // Assignment
                        if op == "=" {
                            tokens.next();
                            if let Some(Token::Identifier(slot)) = tokens.next() {
                                // For simplicity, we will just parse literals here
                                if let Some(Token::Number(num)) = tokens.next() {
                                    statements.push(Statement::Assign { 
                                        object, 
                                        slot, 
                                        value: Expression::Literal(Value::Single(PrimitiveValue::Int32(num as i32))) 
                                    });
                                } else if let Some(Token::Bool(b)) = tokens.next() {
                                    statements.push(Statement::Assign { 
                                        object, 
                                        slot, 
                                        value: Expression::Literal(Value::Single(PrimitiveValue::Bool(b))) 
                                    });
                                } else if let Some(Token::String(s)) = tokens.next() {
                                    statements.push(Statement::Assign { 
                                        object, 
                                        slot, 
                                        value: Expression::Literal(Value::Single(PrimitiveValue::String(s))) 
                                    });
                                } else {
                                    panic!("Expected literal value after ':=' operator");
                                }
                            } else {
                                panic!("Expected slot name after object name in assignment");
                            }
                        } else {
                            panic!("Unexpected operator after object name: {:?}", op);
                        }
                    } else {
                        panic!("Expected operator after object name");
                    }
                } else {
                    panic!("Expected identifier at start of statement");
                }
            
            },

            _ => {
                panic!("Unexpected token: {:?}", token);
            }
        }
    }
    
    statements
}   
use crate::tokenizer::Token;
use crate::PrimitiveValue;
use crate::Value;
use crate::ValueKind;

type Identifier = String;
pub enum Expression {
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

pub struct Mapping {
    pub from_slot: Identifier,
    pub to_slot: Identifier,
}

pub struct SlotId {
    pub name: Identifier,
    pub value_type: ValueKind,
    pub is_static: bool,
}

pub enum Statement {
    DeclareShape { 
        name: Identifier, 
        slot_ids: Vec<SlotId>,
        mappings: Vec<Mapping>,
    },
    DeclareObject { name: Identifier },
    Attach { object: Identifier, shape: Identifier },
    Detach { object: Identifier, shape: Identifier },
    AddMapping { object: Identifier, from_slot: Identifier, to_slot: Identifier },
    AttachWithRemap { object: Identifier, shape: Identifier, mappings: Vec<Mapping> },
    Print { object: Identifier },
    PrintString { string: String },
    Assign {
        object: Identifier,
        slot: Identifier,
        value: Expression,
    }
}

pub fn parse(input: Vec<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut tokens = input.into_iter().peekable();

    while let Some(token) = tokens.peek() {
        match token {

            // Shape declaration
            Token::Keyword(k) if k == "shape" => {
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

                statements.push(Statement::DeclareShape { name: shape_identifier, slot_ids, mappings });
            },

            // Object declaration
            Token::Keyword(k) if k == "object" => {
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

            // attach
            Token::Keyword(k) if k == "attach" => {
                tokens.next(); // consume 'attach' keyword
                let object = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected object name after 'attach', got {:?}",
                        other
                    ),
                };
                let shape = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected shape name after object name in attach statement, got {:?}",
                        other
                    ),
                };

                statements.push(Statement::Attach { object, shape });
            },

            // detach
            Token::Keyword(k) if k == "detach" => {
                tokens.next(); // consume 'detach' keyword
                let object = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected object name after 'detach', got {:?}",
                        other
                    ),
                };
                let shape = match tokens.next() {
                    Some(Token::Identifier(name)) => name.clone(),
                    other => panic!(
                        "Expected shape name after object name in detach statement, got {:?}",
                        other
                    ),
                };

                statements.push(Statement::Detach { object, shape });
            },

            // print
            Token::Keyword(k) if k == "print" => {
                tokens.next(); // consume 'print' keyword
                match tokens.next() {
                    Some(Token::Identifier(object)) => statements.push(Statement::Print { object }),
                    Some(Token::String(s)) => statements.push(Statement::PrintString { string: s.clone() }),
                    other => panic!(
                        "Expected object name or string after 'print', got {:?}",
                        other
                    ),
                };
            },

            // Object Identifier
            Token::Identifier(_) => {
                if let Some(Token::Identifier(object)) = tokens.next() {
                    if let Some(Token::Operator(op)) = tokens.peek() {

                        // Slot Call
                        match op.as_str() {
                            "." => {
                                tokens.next(); // consume operator
                                let slot = match tokens.next() {
                                    Some(Token::Identifier(name)) => name.clone(),
                                    other => panic!(
                                        "Expected slot name after object name in slot call, got {:?}",
                                        other
                                    ),
                                };

                                let slot_op = match tokens.next() {
                                    Some(Token::Operator(op)) => op.clone(),
                                    other => panic!(
                                        "Expected operator after slot name in slot call, got {:?}",
                                        other
                                    ),
                                };

                                // Assignment
                                match slot_op.as_str() {
                                    "=" => {
                                        let value = match tokens.next() {
                                            Some(Token::Number(num)) => Expression::Literal(Value::Single(PrimitiveValue::Int32(num as i32))),
                                            Some(Token::Bool(b)) => Expression::Literal(Value::Single(PrimitiveValue::Bool(b))),
                                            Some(Token::String(s)) => Expression::Literal(Value::Single(PrimitiveValue::String(s))),
                                            other => panic!(
                                                "Expected literal value after '=' operator in slot assignment, got {:?}",
                                                other
                                            ),
                                        };

                                        statements.push(Statement::Assign { object, slot, value });
                                    }
                                    "." => {
                                        // For now, we only support simple slot access like `object.slot`, but this could be extended to support more complex expressions in the future.
                                        panic!("Nested slot access is not supported yet");
                                    }
                                    other => { panic!("Unexpected operator in slot call: {:?}", other); }
                                }
                            }
                            
                            // Attach
                            ":=" => {
                                tokens.next(); // consume operator
                                let shape = match tokens.next() {
                                    Some(Token::Identifier(name)) => name.clone(),
                                    other => panic!(
                                        "Expected shape name after ':=' operator in attach statement, got {:?}",
                                        other
                                    ),
                                };

                                // Check for mappings
                                if let Some(Token::LeftBrace) = tokens.peek() {
                                    tokens.next(); // consume '{'
                                    let mut mappings = Vec::new();

                                    while let Some(token) = tokens.peek() {
                                        match token {
                                            // From slot mapping
                                            Token::Identifier(from_slot) => {
                                                let from_slot = from_slot.clone();
                                                tokens.next();

                                                // Expect '->' operator
                                                if let Some(Token::Operator(op)) = tokens.next() {
                                                    if op == "->" {
                                                        let to_slot = match tokens.next() {

                                                            // To slot mapping
                                                            Some(Token::Identifier(name)) => name.clone(),
                                                            other => panic!(
                                                                "Expected slot name after '->' in mapping, got {:?}",
                                                                other
                                                            ),
                                                        };

                                                        mappings.push(Mapping { from_slot, to_slot });
                                                    } else {
                                                        panic!("Expected '->' operator in mapping, got {:?}", op);
                                                    }
                                                } else {
                                                    panic!("Expected operator in mapping, got {:?}", tokens.peek());
                                                }
                                            },
                                            Token::RightBrace => {
                                                tokens.next(); // consume '}'
                                                break;
                                            },
                                            _ => {
                                                panic!("Unexpected token in mappings: {:?}", token);
                                            }
                                        }
                                    }

                                    statements.push(Statement::AttachWithRemap { object, shape, mappings });
                                } else {
                                    statements.push(Statement::Attach { object, shape });
                                }
                            }

                            //Detach
                            "=:" => {
                                tokens.next(); // consume operator
                                let shape = match tokens.next() {
                                    Some(Token::Identifier(name)) => name.clone(),
                                    other => panic!(
                                        "Expected shape name after '=:' operator in detach statement, got {:?}",
                                        other
                                    ),
                                };

                                statements.push(Statement::Detach { object, shape });
                            }

                            _ => { panic!("Unexpected operator after object name: {:?}", op); }
                        }
                    } else {
                        panic!("Expected operator after object name, got {:?}", tokens.peek());
                    }
                } else {
                    panic!("Expected identifier at start of statement");
                }
            
            },

            Token::EOF => break,

            _ => {
                panic!("Unexpected token: {:?}", token);
            }
        }
    }
    
    statements
}   
use crate::tokenizer::Token;
use crate::PrimitiveValue;
use crate::Value;
use crate::ValueKind;

type Identifier = String;
#[derive(Debug)]
pub enum Expression {
    Literal(Value),
    Array(Vec<Expression>),
    Variable(Identifier),
    UnaryOp {
        operator: String,
        operand: Box<Expression>,
    },
    BinaryOp {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    SlotAccess {
        object: Identifier,
        slot: Identifier,
    },
    CallStack {
        stack: Vec<Identifier>
    }
}

#[derive(Debug)]
pub struct Mapping {
    pub from_slot: Identifier,
    pub to_slot: Identifier,
}

#[derive(Debug)]
pub struct Azimuth {
    pub name: Identifier,
    pub value_type: ValueKind,
    pub is_static: bool,
    pub set_value: Option<Expression>
}


#[derive(Debug)]
pub enum Statement {
    Using { 
        package: String
    },
    DeclareShape { 
        name: Identifier, 
        slot_ids: Vec<Azimuth>,
        mappings: Vec<Mapping>,
    },
    DeclareShapeWithGenerics { 
        name: Identifier, 
        slot_ids: Vec<Azimuth>,
        mappings: Vec<Mapping>,
        generics: Vec<Identifier>,
    },
    DeclareObject { name: Identifier },
    //DeclareFunction { name: Identifier, params: Vec<(Identifier, ValueKind)>, return_type: ValueKind, body: Vec<Statement> },
    //DeclareNonObject{ name: Identifier, value: PrimitiveValue },
    Attach { object: Identifier, shape: Identifier, generics: Vec<ValueKind> },
    Detach { object: Identifier, shape: Identifier },
    AddMapping { object: Identifier, from_slot: Identifier, to_slot: Identifier },
    AttachWithRemap { object: Identifier, shape: Identifier, mappings: Vec<Mapping>, generics: Vec<ValueKind> },
    Print { object: Identifier },
    PrintString { string: String },
    Assign {
        object: Identifier,
        slot: Identifier,
        value: Expression,
    }
}

fn build_expression(tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>) -> Expression {
    if let Some(token) = tokens.next() {
        match token {
            Token::Number(num) => Expression::Literal(Value::Single(PrimitiveValue::Int32(num as i32))),
            Token::Bool(b) => Expression::Literal(Value::Single(PrimitiveValue::Bool(b))),
            Token::String(s) => Expression::Literal(Value::Single(PrimitiveValue::String(s))),
            Token::Identifier(identifier) => build_call_stack(identifier, tokens),
            other => panic!("Unexpected token in expression: {:?}", other),
        }
    } else {
        panic!("Unexpected end of tokens while building expression");
    }
}

fn build_call_stack(first:Identifier, tokens: &mut std::iter::Peekable<std::vec::IntoIter<Token>>) -> Expression {
    let mut call_stack = Vec::new();
    call_stack.push(first);

    while let Some(token) = tokens.peek() {
        match token {
            Token::Identifier(k) => {
                call_stack.push(k.clone() as Identifier);
                tokens.next();
            }
            Token::Operator(k) if k == "." => { tokens.next(); }
            other => break
        }
    }
    Expression::CallStack{ stack: call_stack }
}

pub fn parse(input: Vec<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut tokens = input.into_iter().peekable();

    while let Some(token) = tokens.peek() {
        match token {

            // Package import
            Token::Keyword(k) if k == "using" => {
                tokens.next(); // consume 'using' keyword
                let package = match tokens.next() {
                    Some(Token::String(name)) => name.clone(),
                    other => panic!("Expected package name after 'using', got {:?}", other),
                };

                statements.push(Statement::Using { package: package });
            },

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
                let mut generics = Vec::new();

                // Establish generics
                match tokens.peek() {
                    Some(Token::Operator(k)) if k == "<" => {
                        tokens.next();

                        // Iterate generic types
                        while let Some(token) = tokens.next() {
                            match token {
                                Token::Identifier(generic_name) => {
                                    generics.push(generic_name as Identifier);
                                }
                                Token::Comma => {}
                                Token::Operator(k) if k == ">" => { break; }
                                _ => {
                                    panic!("Unexpected token in generic declaration: {:?}", token);
                                }
                            }
                        }
                    }
                    other => { }
                }

                if let Some(Token::LeftBrace) = tokens.next() {

                    // Define slots
                    while let Some(token) = tokens.peek() {
                        match token {
                            Token::Identifier(slot_name) => {
                                let slot_name = slot_name.clone();
                                tokens.next();
                                        
                                let mut set_value = None;

                                match tokens.next(){
                                    Some(Token::Type(value_type)) => {
                                        let value_type = value_type.clone();
                                        let is_static = if let Some(Token::Keyword(k)) = tokens.peek() {
                                            if k == "static" {
                                                tokens.next();

                                                // set static value
                                                match tokens.peek() {
                                                    Some(Token::Operator(k)) if k == "=" => {
                                                        tokens.next();
                                                        set_value = Some(build_expression(&mut tokens));
                                                    }
                                                    _ => {}
                                                }

                                                true
                                            } else { false }
                                        } else { false };

                                        slot_ids.push(Azimuth { name: slot_name, value_type, is_static, set_value });
                                    }
                                    Some(Token::Identifier(generic)) if generics.contains(&generic) => {
                                        let value_type = ValueKind::Generic(generics.iter().position(|x| x == &generic).unwrap() as u8);

                                        let is_static = if let Some(Token::Keyword(k)) = tokens.peek() {
                                            if k == "static" {
                                                tokens.next();

                                                // set static value
                                                match tokens.peek() {
                                                    Some(Token::Operator(k)) if k == "=" => {
                                                        tokens.next();
                                                        set_value = Some(build_expression(&mut tokens));
                                                    }
                                                    _ => {}
                                                }
                                                
                                                true
                                            } else { false }
                                        } else { false };

                                        slot_ids.push(Azimuth { name: slot_name, value_type, is_static, set_value });
                                    },
                                    Some(Token::Identifier(shape)) => {
                                        // NEED SHAPE ID IDENTIFIER CHECKING HERE
                                        let value_type = ValueKind::ObjectId(0);

                                        let is_static = if let Some(Token::Keyword(k)) = tokens.peek() {
                                            if k == "static" {
                                                tokens.next();

                                                // set static value
                                                match tokens.peek() {
                                                    Some(Token::Operator(k)) if k == "=" => {
                                                        tokens.next();
                                                        set_value = Some(build_expression(&mut tokens));
                                                    }
                                                    _ => {}
                                                }
                                                
                                                true
                                            } else { false }
                                        } else { false };

                                        slot_ids.push(Azimuth { name: slot_name, value_type, is_static, set_value });
                                    }
                                    _ => {
                                        panic!("Expected type for slot '{}'", slot_name);
                                    }
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

                if(generics.len() > 0) {
                    statements.push(Statement::DeclareShapeWithGenerics { name: shape_identifier, slot_ids, mappings, generics });
                } else {
                    statements.push(Statement::DeclareShape { name: shape_identifier, slot_ids, mappings });
                }
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
                                        let value = build_expression(&mut tokens);
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

                                let mut generics = Vec::new();

                                // Check for generics
                                match tokens.peek(){
                                    Some(Token::Operator(k)) if k == "<" => {
                                        tokens.next(); // consume '<'
                                        // Iterate generic types
                                        while let Some(token) = tokens.next() {
                                            match token {
                                                Token::Type(kind) => {
                                                    generics.push(kind);
                                                }
                                                Token::Comma => {}
                                                Token::Operator(k) if k == ">" => { break; }
                                                _ => {
                                                    panic!("Unexpected token in generic assignment: {:?}", token);
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }

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

                                    statements.push(Statement::AttachWithRemap { object, shape, mappings, generics });
                                } else {
                                    statements.push(Statement::Attach { object, shape, generics });
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
    
    println!("{:?}", statements);
    statements
}   
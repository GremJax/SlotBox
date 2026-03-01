use crate::executor::OBJECT_INSTANCE;
use crate::tokenizer::{self, Keyword, Operator, Span, Token, TokenKind, UNARY_OPERATORS};
use crate::{Function, ValueKind, Value};

type Identifier = String;

#[derive(Debug, Clone)]
pub enum ParseError {
    Error { span: Span, message: String },
    UnexpectedToken { span: Span, token: TokenKind, loc: String },
    IncorrectToken { span: Span, token: TokenKind, expected: String, loc: String },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Error { span, message } =>
                write!(f, "{}: {}", span, message),

            ParseError::UnexpectedToken { span, token, loc } => 
                write!(f, "{}: Unexpected token in {}: {:?}", span, loc, token),

            ParseError::IncorrectToken { span, token, expected, loc } => 
                write!(f, "{}: Incorrect token in {}: {:?}, expected {}", span, loc, token, expected),
        }
    }
}

#[derive(Debug, Clone)]
enum Presence {
    Guaranteed,
    Optional,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Span, Value),
    Array(Span, Vec<Expression>, Option<ValueKind>),
    Variable(Span, Identifier),
    Option(Span, Box<Option<Expression>>),
    UnaryOp {
        span: Span,
        operator: Operator,
        operand: Box<Expression>,
    },
    BinaryOp {
        span: Span,
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Ternary {
        span: Span,
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    MemberAccess {
        span: Span,
        target: Box<Expression>,
        qualifier: Option<ShapeExpression>,
        member: Identifier,
    },
    ArrayAccess {
        span: Span,
        target: Box<Expression>,
        index: Box<Expression>,
    },
    FunctionCall {
        span: Span,
        caller: Box<Expression>,
        target: Box<Expression>,
        args: Vec<Expression>,
    },
    Function {
        span: Span,
        has_self: bool,
        input_types: Vec<RawFunctionParam>,
        output_type: ShapeExpression,
        func: Box<Statement>
    }
}

#[derive(Debug, Clone)]
pub struct RawFunctionParam {
    pub value_type: ShapeExpression,
    pub identifier: Identifier
}

#[derive(Debug, Clone)]
pub struct RawFunction {
    pub has_self: bool,
    pub input_types: Vec<RawFunctionParam>,
    pub output_type: ShapeExpression,
    pub func: Statement
}

#[derive(Debug, Clone)]
pub enum ShapeExpression {
    Shape(Identifier),
    Primitive(ValueKind),
    Array(Box<ShapeExpression>),
    Applied {
        base: Identifier,
        args: Vec<ShapeExpression>,
    },
    Function{func: Box<RawFunction>},
}

#[derive(Debug, Clone)]
pub struct Mapping {
    pub from_slot: Identifier,
    pub to_slot: Identifier,
}

#[derive(Debug, Clone)]
pub struct RawAzimuth {
    pub name: Identifier,
    pub value_type: ShapeExpression,
    pub is_static: bool,
    pub set_value: Option<Expression>
}

#[derive(Debug, Clone)]
pub enum Statement {
    Using { span: Span, package: String },
    DeclareShape { 
        span: Span, 
        name: Identifier, 
        slot_ids: Vec<RawAzimuth>,
        parents: Vec<Identifier>,
        mappings: Vec<Mapping>,
        generics: Vec<ShapeExpression>,
    },
    DeclareObject { span: Span, name: Identifier, shape: ShapeExpression }, 
    DeclareLocal { span: Span, name: Identifier, value: Expression }, 
    Attach { span: Span, object: Expression, shape: ShapeExpression },
    Detach { span: Span, object: Expression, shape: ShapeExpression },
    AddMapping { span: Span, object: Expression, mapping: Mapping },
    AttachWithRemap { span: Span, object: Expression, shape: ShapeExpression, mappings: Vec<Mapping> },
    Print { span: Span, expr: Expression },
    Expression { span: Span, expr: Expression },
    If {
        span: Span, 
        condition: Expression,
        true_statement: Box<Statement>,
        else_statement: Box<Option<Statement>>,
    },
    While {
        span: Span, 
        condition: Expression,
        statement: Box<Statement>,
    },
    For {
        span: Span, 
        local: Identifier,
        target: Expression,
        statement: Box<Statement>,
    },
    Assign { span: Span, target: Expression, value: Expression, },
    AssignAugmented { span: Span, target: Expression, value: Expression, operator: Operator },
    Block(Vec<Statement>),

    Break{ span: Span }, 
    Continue{ span: Span }, 
    Return{ span: Span, value: Expression },
    Throw{ span: Span, message: Expression },
}

type PeekableTokens = std::iter::Peekable<std::vec::IntoIter<Token>>;

fn parse_expression(tokens: &mut PeekableTokens) -> Result<Expression, ParseError> {
    let token = tokens.next().unwrap();
    let span = token.span.clone();

    let mut expr = match token.kind {
        // Unary preceding operator
        TokenKind::Operator(operator) if UNARY_OPERATORS.contains(&operator) => {
            let expr = parse_expression(tokens)?;
            Expression::UnaryOp{span, operator, operand: Box::new(expr)}
        } 

        TokenKind::Number(num) => Expression::Value(span, (num as i32).into()),
        TokenKind::Bool(b) => Expression::Value(span, b.into()),
        TokenKind::String(s) => Expression::Value(span, s.into()),

        TokenKind::Identifier(identifier) => Expression::Variable(span, identifier),
        TokenKind::Keyword(Keyword::PSelf) => Expression::Variable(span, format!("self")),

        TokenKind::LeftParen => {
            let expr = parse_expression(tokens)?;
            tokens.next(); // Consume right parenthesis
            expr
        }

        // Array literal
        TokenKind::LeftBracket => {
            let mut elements = Vec::new();

            while let Some(token) = tokens.peek() {
                if matches!(token.kind, TokenKind::RightBracket) {
                    tokens.next();
                    break;
                }

                elements.push(parse_expression(tokens)?);

                if let Some(token) = tokens.peek()
                    && matches!(token.kind, TokenKind::Comma) {
                    tokens.next();
                }
            }

            Expression::Array(span, elements, None)
        }

        token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("value expression") }),
    };

    let mut caller;
    let mut qualifier = None;

    // Check for member access / function call / array index
    while let Some(token) = tokens.peek() {
        let span = token.span.clone();

        match token.kind {
            TokenKind::Operator(Operator::DColon) => {
                tokens.next(); // Consume dcolon
                qualifier = Some(parse_shape_expression(tokens)?);
            }
            TokenKind::Operator(Operator::Dot) => {
                tokens.next(); // Consume dot

                if let TokenKind::Identifier(k) = tokens.next().unwrap().kind {
                    // Access Member
                    caller = expr.clone();
                    expr = Expression::MemberAccess{span, target: Box::new(expr), qualifier:qualifier.clone(), member: k };
                    qualifier = None;

                    // Check for function call
                    if matches!(tokens.peek().unwrap().kind, TokenKind::LeftParen){
                        let token = tokens.next().unwrap();
                        let span = token.span.clone();

                        let mut args = Vec::new();

                        while let Some(token) = tokens.peek() {
                            match token.kind {
                                TokenKind::RightParen => {
                                    tokens.next();
                                    break
                                }
                                TokenKind::Comma => {
                                    tokens.next();
                                }
                                _ => {
                                    args.push(parse_expression(tokens)?);
                                }
                            }
                        }

                        expr = Expression::FunctionCall{span, caller:Box::new(caller), target:Box::new(expr), args};
                    }
                }
            }
            
            TokenKind::LeftBracket => {
                tokens.next(); // Consume bracket
                let index = parse_expression(tokens)?;
                tokens.next(); // Consume bracket
                expr = Expression::ArrayAccess{span, target:Box::new(expr), index:Box::new(index)};
            }

            _ => break,
        }
    }

    // Check for operators
    if let Some(token) = tokens.peek() {
        let span = token.span.clone();
        if matches!(&token.kind, TokenKind::Operator(op) 
            if tokenizer::BINARY_OPERATORS.contains(&op))
        {
            if let TokenKind::Operator(op) = tokens.next().unwrap().kind {
                return Ok(Expression::BinaryOp {span, 
                    left: Box::new(expr),
                    operator: op,
                    right: Box::new(parse_expression(tokens)?),
                });
            }
        }
    }

    // Check for ternary
    if let Some(token) = tokens.peek() {
        let span = token.span.clone();
        if matches!(&token.kind, TokenKind::Operator(Operator::Question))
        {
            tokens.next(); // consume question mark
            
            let true_expr = parse_expression(tokens)?;

            // Expect colon
            let token = tokens.peek().unwrap();
            if !matches!(token.kind, TokenKind::Operator(Operator::Colon)) {
                let span = token.span.clone();
                return Err(ParseError::IncorrectToken { span, token:token.kind.clone(), expected: format!(":"), loc: format!("Ternary operation") });
            }
            tokens.next(); // Consume colon
            
            let else_expr = parse_expression(tokens)?;

            expr = Expression::Ternary{ span, condition:Box::new(expr), true_expr:Box::new(true_expr), else_expr:Box::new(else_expr)}
        }
    }

    Ok(expr)
}

fn parse_shape_expression(tokens: &mut PeekableTokens) -> Result<ShapeExpression, ParseError> {
    let token = tokens.next().unwrap();
    let span = token.span.clone();

    let mut base = match token.kind {
        TokenKind::Identifier(identifier) => ShapeExpression::Shape(identifier),
        TokenKind::Type(kind) => ShapeExpression::Primitive(kind),
        other => return Err(ParseError::UnexpectedToken { span, token:other, loc:format!("shape expression") }),
    };

    if let TokenKind::Operator(Operator::LT) = tokens.peek().unwrap().kind {
        tokens.next(); // consume '<'
        let mut args = Vec::new();

        loop {
            let arg = parse_shape_expression(tokens)?;
            args.push(arg);

            match tokens.next().unwrap().kind {
                TokenKind::Comma => continue,
                TokenKind::Operator(Operator::GT) => break,
                token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("shape generics") }),
            }
        }

        let base_identifier = match base {
            ShapeExpression::Shape(identifier) => identifier,
            other => todo!()
        };

        base = ShapeExpression::Applied {
            base: base_identifier,
            args,
        }
    }

    if let TokenKind::LeftBracket = tokens.peek().unwrap().kind {
        tokens.next(); // consume '['
        let token = tokens.peek().unwrap();
        if let TokenKind::RightBracket = token.kind {
            tokens.next(); // consume ']'
        } else { return Err(ParseError::IncorrectToken { span, token: token.kind.clone(), expected: format!("]"), loc: format!("shape array definition")}) }
        
        base = ShapeExpression::Array(Box::new(base));
    }

    Ok(base)
}

fn parse_function(shape: ShapeExpression, is_static: bool, tokens: &mut PeekableTokens) -> Result<RawFunction, ParseError> {
    tokens.next(); // Consume LParen

    let mut input_types = Vec::new();

    let has_self = if !is_static {
        // Add self param
        let self_param = RawFunctionParam{value_type: shape, identifier:format!("self")};
        input_types.push(self_param);

        true

    } else if matches!(tokens.peek().unwrap().kind, TokenKind::Keyword(Keyword::PSelf)){
        tokens.next(); // Consume self keyword

        // Add self param
        let self_param = RawFunctionParam{value_type: shape, identifier:format!("self")};
        input_types.push(self_param);

        true
    } else { false };

    while let Some(token) = tokens.peek() {
        let span = token.span.clone();
        match token.kind {
            TokenKind::Comma => {
                tokens.next();
            },
            TokenKind::RightParen => {
                tokens.next();
                break
            }
            _ => { 
                let value_type = parse_shape_expression(tokens)?;
                let identifier = match tokens.next().unwrap().kind {
                    TokenKind::Identifier(k) => k,
                    token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("function parameter definition") }),
                };
                input_types.push(RawFunctionParam{value_type, identifier});
            }
        }
    }

    // Expect arrow
    let token = tokens.peek().unwrap();
    let output_type = if matches!(token.kind, TokenKind::Operator(Operator::Arrow)) {
        tokens.next(); // Consume arrow
        // Return type
        parse_shape_expression(tokens)?
    } else { ShapeExpression::Primitive(ValueKind::None) };

    // Statement
    let func = parse_statement(tokens)?;

    Ok(RawFunction{ has_self, input_types, output_type, func })
}

fn parse_object_statement(span:Span, tokens: &mut PeekableTokens) -> Result<Statement, ParseError> {
    // Build member call
    let object = parse_expression(tokens)?;
    
    let token = tokens.peek().unwrap();
    match &token.kind {
        // Attach
        TokenKind::Operator(Operator::Attach) => {
            tokens.next(); // consume operator
            
            let shape = parse_shape_expression(tokens)?;

            // Check for mappings
            if let TokenKind::LeftParen = tokens.peek().unwrap().kind {
                tokens.next(); // consume '{'
                let mut mappings = Vec::new();

                while let Some(token) = tokens.next() {
                    match token.kind {
                        // From slot mapping
                        TokenKind::Identifier(from_slot) => {

                            // Expect '->' operator
                            match tokens.next().unwrap() {
                                token if matches!(token.kind, TokenKind::Operator(Operator::Arrow)) => {

                                    // Expect to slot identifier
                                    match tokens.next().unwrap().kind {
                                        TokenKind::Identifier(to_slot) => mappings.push(Mapping { from_slot, to_slot }),
                                        token => return Err(ParseError::IncorrectToken { span, token, expected:format!("Shape"), loc:format!("shape attachment remap value") }),
                                    }

                                },
                                token => return Err(ParseError::IncorrectToken { span, token:token.kind, expected:format!("->"), loc:format!("shape attachment remap value") }),
                            }
                        },
                        TokenKind::RightParen => break,
                        token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("shape attachment remap") }),
                    }
                }

                Ok(Statement::AttachWithRemap {span, object, shape, mappings })
            } else {
                Ok(Statement::Attach {span, object, shape })
            }
        }

        //Detach
        TokenKind::Operator(Operator::Detach) => {
            tokens.next(); // consume operator
            
            let shape = parse_shape_expression(tokens)?;

            Ok(Statement::Detach {span, object, shape })
        }

        // Assign
        TokenKind::Operator(Operator::Assign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::Assign{ span, target: object, value })
        }
        TokenKind::Operator(Operator::AddAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::Add })
        }
        TokenKind::Operator(Operator::SubAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::Sub })
        }
        TokenKind::Operator(Operator::MulAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::Mul })
        }
        TokenKind::Operator(Operator::DivAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::Div })
        }
        TokenKind::Operator(Operator::ModAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::Mod })
        }
        TokenKind::Operator(Operator::AndAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::BWAnd })
        }
        TokenKind::Operator(Operator::OrAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::BWOr })
        }
        TokenKind::Operator(Operator::XorAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::BWXor })
        }
        TokenKind::Operator(Operator::ShiftLAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::BWShiftL })
        }
        TokenKind::Operator(Operator::ShiftRAssign) => {
            tokens.next(); // consume operator
            let value = parse_expression(tokens)?;
            Ok(Statement::AssignAugmented{ span, target: object, value, operator:Operator::BWShiftR })
        }
        TokenKind::Operator(Operator::Inc) => {
            tokens.next(); // consume operator
            let one = Expression::Value(span.clone(), 1.into());
            Ok(Statement::AssignAugmented{ span, target: object, value:one, operator:Operator::Add })
        }
        TokenKind::Operator(Operator::Dec) => {
            tokens.next(); // consume operator
            let one = Expression::Value(span.clone(), (-1).into());
            Ok(Statement::AssignAugmented{ span, target: object, value:one, operator:Operator::Add })
        }

        _ => Ok(Statement::Expression{span, expr:object })

        //token => return Err(ParseError::UnexpectedToken { span, token:token.clone(), loc:format!("object operation") }),
    }
}

fn parse_statement(tokens: &mut PeekableTokens) -> Result<Statement, ParseError> {
    let token = tokens.peek().unwrap();
    let span = token.span.clone();

    match &token.kind {

        // Package import
        TokenKind::Keyword(Keyword::Using) => {
            tokens.next(); // consume 'using' keyword
            let package = match tokens.next().unwrap().kind {
                TokenKind::String(name) => name.clone(),
                token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("module import") }),
            };

            Ok(Statement::Using {span, package: package })
        },

        // Shape declaration
        TokenKind::Keyword(Keyword::Shape) => {
            tokens.next(); // consume 'shape' keyword
            let shape_identifier = match tokens.next().unwrap().kind {
                TokenKind::Identifier(name) => name.clone(),
                token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("shape declaration") }),
            };

            let mut generics = Vec::new();
            //let mut generic_names = Vec::new();

            // Establish generics
            if let TokenKind::Operator(Operator::LT) = tokens.peek().unwrap().kind {
                tokens.next(); // consume '<'

                while let Some(token) = tokens.peek() {
                    match &token.kind {
                        TokenKind::Identifier(_) => {
                            let shape_expr = parse_shape_expression(tokens)?;
                            generics.push(shape_expr)
                        }
                        TokenKind::Comma => { tokens.next(); }
                        TokenKind::Operator(Operator::GT) => { 
                            tokens.next();
                            break
                        }
                        token => return Err(ParseError::UnexpectedToken { span, token:token.clone(), loc:format!("shape generic declaration") }),
                    }
                }
            }
            
            // Establish inheritance
            let mut mappings = Vec::new();
            let mut parents = Vec::new();
            
            let token = tokens.peek().unwrap();
            if let TokenKind::Operator(Operator::Colon) = token.kind {
                tokens.next(); // Consume colon

                while let Some(token) = tokens.peek() {
                    let span = token.span.clone();

                    match &token.kind {
                        TokenKind::Comma => { tokens.next(); }
                        TokenKind::Identifier(parent) => {
                            parents.push(parent.clone());
                            tokens.next();

                            // Check for mappings
                            if let TokenKind::LeftParen = tokens.peek().unwrap().kind {
                                tokens.next(); // consume '{'

                                while let Some(token) = tokens.next() {
                                    match token.kind {
                                        // From slot mapping
                                        TokenKind::Identifier(from_slot) => {

                                            // Expect '->' operator
                                            let token = tokens.next().unwrap();
                                            if !matches!(token.kind, TokenKind::Operator(Operator::Arrow)) {
                                                return Err(ParseError::IncorrectToken { span, token:token.kind, expected:format!("->"), loc:format!("shape inheritance remap value") })
                                            }
                                            
                                            // Expect to slot identifier
                                            match tokens.next().unwrap().kind {
                                                TokenKind::Identifier(to_slot) => mappings.push(Mapping { from_slot, to_slot }),
                                                token => return Err(ParseError::IncorrectToken { span, token, expected:format!("Slot"), loc:format!("shape inheritance remap value") }),
                                            }
                                        },
                                        TokenKind::RightParen => break,
                                        token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("shape inheritance remap") }),
                                    }
                                }
                            }
                        },
                        _ => break,
                    }
                }

            }

            // Define slots
            let mut slot_ids = Vec::new();

            let token = tokens.next().unwrap();
            if let TokenKind::LeftBrace = token.kind {

                // Define slots
                while let Some(token) = tokens.peek() {
                    if matches!(token.kind, TokenKind::RightBrace) {
                        tokens.next();
                        break;
                    }

                    // Static
                    let is_static = match tokens.peek().unwrap().kind {
                        TokenKind::Keyword(Keyword::Static) => {
                            tokens.next();
                            true
                        }
                        _ => false
                    };

                    // Const
                    let is_const = match tokens.peek().unwrap().kind {
                        TokenKind::Keyword(Keyword::Const) => {
                            tokens.next();
                            true
                        }
                        _ => false
                    };

                    // Abstract
                    let is_abstract = match tokens.peek().unwrap().kind {
                        TokenKind::Keyword(Keyword::Abstract) => {
                            tokens.next();
                            true
                        }
                        _ => false
                    };

                    // Locked
                    let is_locked = match tokens.peek().unwrap().kind {
                        TokenKind::Keyword(Keyword::Locked) => {
                            tokens.next();
                            true
                        }
                        _ => false
                    };

                    // Slot name
                    let token = tokens.next().unwrap();
                    let slot_name = match token.kind {
                        TokenKind::Identifier(k) => k,
                        other => return Err(ParseError::UnexpectedToken { span:token.span, token:other, loc: format!("azimuth declaration") }),
                    };

                    // Function check
                    if matches!(tokens.peek().unwrap().kind, TokenKind::LeftParen) {
                        
                        // Function
                        let func = parse_function(ShapeExpression::Shape(shape_identifier.clone()), is_static, tokens)?;
                        let value_type = ShapeExpression::Function{func:Box::new(func.clone())};
                        
                        let func_expr = Expression::Function{ 
                            span:span.clone(), 
                            has_self:func.has_self,
                            input_types: func.input_types, 
                            output_type: func.output_type, 
                            func:Box::new(func.func) 
                        };

                        slot_ids.push(RawAzimuth { 
                            name: slot_name, 
                            value_type,
                            is_static: true, 
                            set_value: Some(func_expr), 
                        });
                        continue;
                    }

                    // Type
                    let value_type = parse_shape_expression(tokens)?;

                    // Default
                    let set_value = match tokens.peek().unwrap().kind {
                        TokenKind::Operator(Operator::Assign) => {
                            tokens.next();
                            Some(parse_expression(tokens)?)
                        }
                        _ => None
                    };

                    slot_ids.push(RawAzimuth { name: slot_name, value_type, is_static, set_value });
                }
            } else {
                return Err(ParseError::IncorrectToken{span, token:token.kind, expected:format!("{{"), loc:format!("shape declaration")});
            }

            Ok(Statement::DeclareShape { span, name: shape_identifier, slot_ids, parents, mappings, generics })
        },

        // Object declaration
        TokenKind::Keyword(Keyword::Let) => {
            tokens.next(); // consume 'let' keyword
            let object_identifier = match tokens.next().unwrap().kind {
                TokenKind::Identifier(name) => name.clone(),
                token => return Err(ParseError::UnexpectedToken { span, token, loc:format!("object declaration") }),
            };

            let token = tokens.peek().unwrap();
            match token.kind {
                TokenKind::Operator(Operator::Attach) => {
                    // Object with attachment
                    tokens.next();
                    let shape = parse_shape_expression(tokens)?;
                    Ok(Statement::DeclareObject {span, name: object_identifier, shape })
                }
                TokenKind::Operator(Operator::Assign) => {
                    // Local with assignment
                    tokens.next();
                    let value = parse_expression(tokens)?;
                    Ok(Statement::DeclareLocal {span, name: object_identifier, value })
                }
                _ => {
                    // Object without assignment
                    Ok(Statement::DeclareObject {span, name: object_identifier, shape:ShapeExpression::Primitive(ValueKind::Shape(OBJECT_INSTANCE)) })
                }
            }
        },

        // print
        TokenKind::Keyword(Keyword::Print) => {
            tokens.next(); // consume 'print' keyword
            Ok(Statement::Print{span, expr: parse_expression(tokens)?})
        },

        // Object Identifier
        TokenKind::Keyword(Keyword::PSelf) => parse_object_statement(span, tokens),
        TokenKind::Identifier(_) => parse_object_statement(span, tokens),

        // Blocking
        TokenKind::LeftBrace => {
            tokens.next(); // Consume brace

            // Add current statements to stack
            let mut statements = Vec::new();
            while let Some(token) = tokens.peek() {
                match token.kind {
                    TokenKind::RightBrace => {
                        tokens.next(); // Consume brace
                        break
                    }
                    TokenKind::EOF => { return Err(ParseError::IncorrectToken { span, token:token.kind.clone(), expected:format!("}}"), loc:format!("block") }); }
                    _ => statements.push(parse_statement(tokens)?)
                }
            }

            Ok(Statement::Block(statements))
        },

        // If
        TokenKind::Keyword(Keyword::If) => {
            tokens.next(); // Consume if

            let condition = parse_expression(tokens)?;
            let true_statement = parse_statement(tokens)?;

            let else_token = tokens.peek().unwrap();
            let else_statement = match else_token.kind {
                TokenKind::Keyword(Keyword::Else) => {
                    tokens.next(); // Consume else
                    Some(parse_statement(tokens)?)
                }
                _ => None
            };

            Ok(Statement::If{ span, condition, true_statement: Box::new(true_statement), else_statement: Box::new(else_statement) })
        },

        // While
        TokenKind::Keyword(Keyword::While) => {
            tokens.next(); // Consume while

            let condition = parse_expression(tokens)?;
            let statement = parse_statement(tokens)?;

            Ok(Statement::While{span, condition, statement: Box::new(statement) })
        },

        // Loop (while true)
        TokenKind::Keyword(Keyword::Loop) => {
            tokens.next(); // Consume loop

            let condition = Expression::Value(span.clone(), true.into());
            let statement = parse_statement(tokens)?;

            Ok(Statement::While{span, condition, statement: Box::new(statement) })
        },
        
        // For
        TokenKind::Keyword(Keyword::For) => {
            tokens.next(); // Consume keyword

            // Expect local identifier
            let local = match tokens.next().unwrap().kind {
                TokenKind::Identifier(k) => k,
                token => return Err(ParseError::IncorrectToken { span, token, expected: format!("identifier"), loc: format!("For loop header") }),
            };
            
            // Expect in keyword
            match tokens.next().unwrap().kind {
                TokenKind::Keyword(Keyword::In) => {}
                token => return Err(ParseError::IncorrectToken { span, token, expected: format!("in"), loc: format!("For loop header") }),
            };

            let target = parse_expression(tokens)?;

            let statement = parse_statement(tokens)?;

            Ok(Statement::For{span, local, target, statement:Box::new(statement) })
        },

        // Returns
        TokenKind::Keyword(Keyword::Break) => {
            tokens.next();
            Ok(Statement::Break{span})
        }
        TokenKind::Keyword(Keyword::Continue) => {
            tokens.next();
            Ok(Statement::Continue{span})
        }
        TokenKind::Keyword(Keyword::Return) => {
            tokens.next();
            Ok(Statement::Return{span, value:parse_expression(tokens)?})
        }
        TokenKind::Keyword(Keyword::Throw) => {
            tokens.next();
            Ok(Statement::Throw{span, message:parse_expression(tokens)?})
        }

        token => Err(ParseError::UnexpectedToken { span, token:token.clone(), loc:format!("statements") }),
    }
}

pub fn parse(input: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::new();
    let mut tokens = input.into_iter().peekable();

    while let Some(token) = tokens.peek() {
        match token.kind {
            TokenKind::EOF => break,
            _ => statements.push(parse_statement(&mut tokens)?)
        }
    }
    
    println!("\n Parsed Ast: \n{:?}", statements);
    Ok(statements)
}
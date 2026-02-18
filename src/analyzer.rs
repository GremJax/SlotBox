use std::{collections::HashMap, fs};

use crate::{AzimuthId, ObjectId, PrimitiveValue, ShapeId, Value, ValueKind, analyzer, executor::{self, ShapeInstance}, parser::{self, Mapping}, tokenizer::{self, Span}}; 
use parser::{Azimuth, Expression, Statement, ShapeExpression};
use tokenizer::{Operator};

#[derive(Debug, Clone)]
pub enum CompileError {
    Error { span: Span, message: String },
    UndefinedSymbol { span: Span, name: String },
    UndefinedStatic { span: Span, name: String },
    DuplicateSymbol { span: Span, name: String },
    ExpectedBoolCondition { span: Span, found: ValueKind },
    TypeMismatch { span: Span, expected: ValueKind, found: ValueKind },
    InvalidUnaryOp { span: Span, operator: Operator, operand: Value },
    InvalidBinaryOp { span: Span, operator: Operator, left: Value, right: Value },
    InvalidThrow { span: Span, found: ValueKind },
    IoError { span: Span, message: String },
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Error { span, message } =>
                write!(f, "{}: {}", span, message),

            CompileError::UndefinedSymbol { span, name } =>
                write!(f, "{}: Undefined symbol: {}", span, name),

            CompileError::DuplicateSymbol { span, name } =>
                write!(f, "{}: Duplicate symbol: {}", span, name),

            CompileError::ExpectedBoolCondition { span, found } =>
                write!(f, "{}: Expected bool condition, got {:?}", span, found),

            CompileError::TypeMismatch { span, expected, found } =>
                write!(f, "{}: Type mismatch: expected {:?}, got {:?}", span, expected, found),

            CompileError::InvalidUnaryOp { span, operator, operand } =>
                write!(f, "{}: Invalid unary operator {:?} on {:?}", span, operator, operand),

            CompileError::InvalidBinaryOp { span, operator, left, right } =>
                write!(f, "{}: Invalid binary operator {:?} on {:?} and {:?}", span, operator, left, right),

            CompileError::InvalidThrow { span, found } =>
                write!(f, "{}: Throw expects string, got {:?}", span, found),

            CompileError::UndefinedStatic { span, name } =>
                write!(f, "{}: Static not found for {}", span, name),

            CompileError::IoError { span, message } =>
                write!(f, "{}: IO error: {}", span, message),
        }
    }
}

type LocalId = u32;
type Identifier = String;

#[derive(Debug, Clone)]
pub struct ShapeInfo {
    pub id: ShapeId,
    pub name: Identifier,
    pub static_id: Box<Option<Symbol>>,
    pub azimuths: Vec<Symbol>,
    pub generics: Vec<ResolvedShapeExpression>,
}

#[derive(Debug, Clone)]
pub struct AzimuthInfo {
    pub id: AzimuthId,
    pub name: Identifier,
    pub is_static: bool,
    pub shape_id: ShapeId,
    pub default_value: Box<Option<ResolvedExpression>>,
    pub value_type: ValueKind,
}

#[derive(Debug, Clone)]
pub struct ObjectInfo {
    pub id: ObjectId,
    pub name: Identifier,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Shape(ShapeInfo),
    Object(ObjectInfo),
    Azimuth(AzimuthInfo),
    Local(LocalId),
}

impl Symbol {
    fn kind(&self) -> ValueKind {
        match self {
            Symbol::Shape(info) => ValueKind::Shape(ShapeInstance{id: info.id, generics: Vec::new()}),
            Symbol::Object(info) => ValueKind::Shape(executor::OBJECT_INSTANCE),
            Symbol::Azimuth(info) => info.value_type.clone(),
            Symbol::Local(info) => ValueKind::None,
        }
    }
}

type ScopeId = u32;

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub symbols: HashMap<Identifier, Symbol>,
}

#[derive(Debug, Clone)]
pub struct ResolvedMapping {
    pub from: Symbol,
    pub to: Symbol,
}

#[derive(Debug, Clone)]
pub enum ResolvedStatement {
    Using { span: Span, ast: Vec<ResolvedStatement>, },
    DeclareShape { span: Span, symbol: Symbol },
    DeclareObject { span: Span, symbol: Symbol },
    Attach { span: Span, object: ResolvedExpression, shape: ResolvedShapeExpression, },
    Detach { span: Span, object: ResolvedExpression, shape: ResolvedShapeExpression },
    AddMapping { span: Span, object: ResolvedExpression, mapping: ResolvedMapping },
    AttachWithRemap { span: Span, object: ResolvedExpression, shape: ResolvedShapeExpression, mappings: Vec<ResolvedMapping>, },
    Print { span: Span, expr: ResolvedExpression },
    If {
        span: Span, 
        condition: ResolvedExpression,
        true_statement: Box<ResolvedStatement>,
        else_statement: Box<Option<ResolvedStatement>>,
    },
    While {
        span: Span, 
        condition: ResolvedExpression,
        statement: Box<ResolvedStatement>,
    },
    For {
        
    },
    Assign { span: Span, target: ResolvedExpression, value: ResolvedExpression },
    Block(Vec<ResolvedStatement>),
    Break{ span: Span, }, 
    Continue{ span: Span, }, 
    Return{span: Span, value: ResolvedExpression },
    Throw{span: Span, message: ResolvedExpression },
}

#[derive(Debug, Clone)]
pub enum ResolvedShapeExpression {
    Simple(Symbol),
    Parameter(Identifier), // T
    Primitive(ValueKind),
    Applied {
        base: Symbol,
        args: Vec<ResolvedShapeExpression>,
    }
}

impl ResolvedShapeExpression {
    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedShapeExpression::Simple(symbol) => symbol.kind(),
            ResolvedShapeExpression::Parameter(_) => ValueKind::Generic(0),
            ResolvedShapeExpression::Applied{base: Symbol::Shape(info), args} => ValueKind::Shape(ShapeInstance{
                id: info.id,
                generics: args.iter().map(|expr| expr.kind()).collect()
            }),
            ResolvedShapeExpression::Primitive(kind) => kind.clone(),
            other => ValueKind::None
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedExpression {
    Value(Span, Value),
    Array(Span, Vec<ResolvedExpression>),
    Variable(Span, Symbol),
    UnaryOp {
        span: Span,
        operator: Operator,
        operand: Box<ResolvedExpression>,
    },
    BinaryOp {
        span: Span,
        left: Box<ResolvedExpression>,
        operator: Operator,
        right: Box<ResolvedExpression>,
    },
    MemberAccess  {
        span: Span,
        target: Box<ResolvedExpression>,
        member: Symbol,
    }
}

impl ResolvedExpression {
    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedExpression::Value(_, Value::Single(value)) => value.kind(),
            ResolvedExpression::Value(_, Value::Array(values)) => todo!(),
            ResolvedExpression::Value(_, Value::Empty) => ValueKind::None,
            ResolvedExpression::Array(_, resolved_expressions) => todo!(),
            ResolvedExpression::Variable(_, symbol) => symbol.kind(),
            ResolvedExpression::UnaryOp {span:_,  operator, operand:_ } => operator.kind(),
            ResolvedExpression::BinaryOp {span:_, left:_, operator, right:_ } => operator.kind(),
            ResolvedExpression::MemberAccess {span:_, target:_, member } => member.kind(),
            _ => todo!()
        }
    }
}

pub fn type_binary_operands(operator: Operator) -> ValueKind {
    match operator {
        Operator::Mul | Operator::Div | Operator::Sub | Operator::Mod |
        Operator::BWAnd | Operator::BWOr | Operator::BWXor | Operator::BWNot | Operator::BWShiftL | Operator::BWShiftR |
        Operator::Inc | Operator::Dec | Operator::Range | Operator::RangeLT
            => ValueKind::Int32,
            
        Operator::And | Operator::Or | Operator::Not
            => ValueKind::Bool,
            
        _ => ValueKind::None
    }
}

pub fn compile_time_error(span: Span, message:String) {
    panic!("[{:?}:{:?}]: {}", span.line, span.column, message);
}

struct Analyzer{
    scopes: Vec<Scope>,
    next_scope_id: u32,
    next_shape_id: u32,
    next_azimuth_id: u32,
    next_object_id: u32,
}

impl Analyzer {
    pub fn new() -> Self {
        let global = Scope{id: 0, parent: None, symbols: HashMap::new()};
        let mut analyzer = Analyzer{
            scopes: Vec::new(), 
            next_scope_id: 1,
            next_shape_id: 0,
            next_azimuth_id: 0,
            next_object_id: 1,
        };
        analyzer.scopes.push(global);
        analyzer
    }

    pub fn get_scope(&self, id:ScopeId) -> &Scope {
        self.scopes.get(id as usize).expect(format!("Scope not found: {:?}", id).as_str())
    }

    pub fn get_scope_mut(&mut self, id:ScopeId) -> &mut Scope {
        self.scopes.get_mut(id as usize).expect(format!("Scope not found: {:?}", id).as_str())
    }

    pub fn create_scope(&mut self, parent:ScopeId) -> ScopeId {
        let id = self.next_scope_id;
        let scope = Scope{id: id.clone(), parent: Some(parent), symbols: HashMap::new()};

        self.next_scope_id += 1;
        self.scopes.push(scope);
        id
    }

    pub fn get_symbol(&self, id:ScopeId, identifier:Identifier) -> Option<&Symbol> {
        let scope = self.get_scope(id);
        match scope.symbols.get(&identifier) {
            Some(symbol) => Some(symbol),
            _ => {
                match scope.parent {
                    Some(parent) => self.get_symbol(parent, identifier),
                    _ => None
                }
            }
        }
    }

    pub fn get_object(&self, id:ScopeId, identifier:Identifier) -> Option<&Symbol> {
        let symbol = self.get_symbol(id, identifier.clone());
        match symbol {
            Some(Symbol::Object(_)) => symbol,
            _ => None,
        }
    }

    pub fn get_shape(&self, id:ScopeId, identifier:Identifier) -> Option<&Symbol> {
        let symbol = self.get_symbol(id, identifier.clone());
        match symbol {
            Some(Symbol::Shape(_)) => symbol,
            _ => None,
        }
    }
    
    fn declare_shape(&mut self, span: Span, scope: ScopeId, name: Identifier, slot_ids: Vec<Azimuth>, mappings: Vec<Mapping>, generics: Vec<ShapeExpression>) -> Result<&Symbol, CompileError> {
        let id = self.next_shape_id.clone();
        self.next_shape_id += 1;

        // Azimuths
        let mut az_symbols = Vec::new();
        let mut az_id = self.next_azimuth_id.clone();
        let mut has_static = false;

        for azimuth in slot_ids {
            let default_value = match azimuth.set_value {
                Some(expr) => Some(self.resolve_expression(expr, scope)?),
                _ => None,
            };

            let symbol = Symbol::Azimuth(AzimuthInfo {
                id: az_id,
                name: azimuth.name,
                shape_id: id,
                default_value: Box::new(default_value),
                is_static: azimuth.is_static,
                value_type: self.resolve_shape_expression(azimuth.value_type, scope)?.kind(),
            });
            if azimuth.is_static { has_static = true; }
            az_symbols.push(symbol);
            az_id += 1;
        }
        self.next_azimuth_id = az_id;

        // Generics
        let mut resolved_generics = Vec::new();
        for generic in generics {
            resolved_generics.push(self.resolve_shape_expression(generic, scope)?);
        }

        // Static singleton
        let static_info = if has_static {
            Some(self.declare_object(scope, format!("{}::Static", name)).clone())
        } else { None };
        
        let scope = self.get_scope_mut(scope);

        for az_symbol in &az_symbols {
            match &az_symbol {
                Symbol::Azimuth(info) => { scope.symbols.insert(info.name.clone(), az_symbol.clone()); }
                _ => { return Err(CompileError::Error{span, message:format!("WHAT")}) }
            }
        }
        
        // Shape
        let symbol = Symbol::Shape(ShapeInfo{
            id: id,
            name: name.clone(),
            static_id: Box::new(static_info),
            azimuths: az_symbols,
            generics: resolved_generics,
        });

        scope.symbols.insert(name.clone(), symbol);
        Ok(scope.symbols.get(&name).unwrap())
    }

    fn declare_object(&mut self, scope: ScopeId, name: Identifier) -> &Symbol {
        let id = self.next_object_id.clone();
        self.next_object_id += 1;

        let symbol = Symbol::Object(ObjectInfo{id: id, name: name.clone()});
        
        let scope = self.get_scope_mut(scope);
        scope.symbols.insert(name.clone(), symbol);
        scope.symbols.get(&name).unwrap()
    }

    pub fn resolve_expression(&self, expression:Expression, scope:ScopeId) -> Result<ResolvedExpression, CompileError> {
        match expression {
            Expression::Value(span, value) => Ok(ResolvedExpression::Value(span, value)),
            Expression::Array(span, expressions) => {
                let mut values = Vec::new();
                for item in expressions {
                    values.push(self.resolve_expression(item, scope.clone())?);
                }
                Ok(ResolvedExpression::Array(span, values))
            },
            
            Expression::Variable(span, k) => {
                match self.get_symbol(scope, k.clone()) {
                    Some(Symbol::Object(info)) => Ok(ResolvedExpression::Variable(span, Symbol::Object(info.clone()))),
                    Some(Symbol::Shape(info)) => {
                        match *info.static_id.clone() {
                            Some(symbol) => Ok(ResolvedExpression::Variable(span, symbol)),
                            _ => Err(CompileError::UndefinedStatic { span, name: k }),
                        }
                    }
                    _ => Err(CompileError::UndefinedSymbol { span, name: k }),
                }
            }

            Expression::UnaryOp { span, operator, operand } => {
                match self.resolve_expression(*operand, scope)? {
                    // Bool Optimization
                    ResolvedExpression::Value(span, Value::Single(PrimitiveValue::Bool(val))) => match operator {
                        Operator::Not => Ok(ResolvedExpression::Value(span, (!val).into())),
                        operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:val.into()}),
                    },

                    // Int Optimization
                    ResolvedExpression::Value(span, Value::Single(PrimitiveValue::Int32(val))) => match operator {
                        Operator::Inc => Ok(ResolvedExpression::Value(span, (val + 1).into())),
                        Operator::Dec => Ok(ResolvedExpression::Value(span, (val - 1).into())),
                        Operator::BWNot => Ok(ResolvedExpression::Value(span, (!val).into())),
                        operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:val.into()}),
                    },

                    // Default
                    operand => Ok(ResolvedExpression::UnaryOp{span, operator, operand: Box::new(operand)})
                }
            }

            Expression::BinaryOp { span, left, operator, right } => {
                match (self.resolve_expression(*left, scope)?, self.resolve_expression(*right, scope)?) {
                    // Bool Optimization
                    (ResolvedExpression::Value(span, Value::Single(PrimitiveValue::Bool(left))), 
                        ResolvedExpression::Value(_, Value::Single(PrimitiveValue::Bool(right)))) => match operator {
                        Operator::Equal => Ok(ResolvedExpression::Value(span, (left == right).into())),
                        Operator::NEqual => Ok(ResolvedExpression::Value(span, (left != right).into())),
                        Operator::And => Ok(ResolvedExpression::Value(span, (left && right).into())),
                        Operator::Or => Ok(ResolvedExpression::Value(span, (left || right).into())),
                        other => Err(CompileError::InvalidBinaryOp { span, operator:other, left:left.into(), right:right.into() }),
                    },
                    
                    // Int Optimization
                    (ResolvedExpression::Value(span, Value::Single(PrimitiveValue::Int32(left))), 
                        ResolvedExpression::Value(_, Value::Single(PrimitiveValue::Int32(right)))) => match operator {
                        Operator::Equal => Ok(ResolvedExpression::Value(span, (left == right).into())),
                        Operator::NEqual => Ok(ResolvedExpression::Value(span, (left != right).into())),
                        Operator::LT => Ok(ResolvedExpression::Value(span, (left < right).into())),
                        Operator::GT => Ok(ResolvedExpression::Value(span, (left > right).into())),
                        Operator::LTE => Ok(ResolvedExpression::Value(span, (left <= right).into())),
                        Operator::GTE => Ok(ResolvedExpression::Value(span, (left >= right).into())),
                        Operator::Add => Ok(ResolvedExpression::Value(span, (left + right).into())),
                        Operator::Sub => Ok(ResolvedExpression::Value(span, (left - right).into())),
                        Operator::Mul => Ok(ResolvedExpression::Value(span, (left * right).into())),
                        Operator::Div => Ok(ResolvedExpression::Value(span, (left / right).into())),
                        Operator::Mod => Ok(ResolvedExpression::Value(span, (left % right).into())),
                        Operator::BWAnd => Ok(ResolvedExpression::Value(span, (left & right).into())),
                        Operator::BWOr => Ok(ResolvedExpression::Value(span, (left | right).into())),
                        Operator::BWXor => Ok(ResolvedExpression::Value(span, (left ^ right).into())),
                        Operator::BWShiftL => Ok(ResolvedExpression::Value(span, (left << right).into())),
                        Operator::BWShiftR => Ok(ResolvedExpression::Value(span, (left >> right).into())),
                        Operator::Range => Ok(ResolvedExpression::Value(span, executor::create_range(left, right))),
                        Operator::RangeLT => Ok(ResolvedExpression::Value(span, executor::create_range(left, right - 1))),
                        operator => Err(CompileError::InvalidBinaryOp { span, operator, left:left.into(), right:right.into() }),
                    },

                    // Default
                    (left, right) => {
                        Ok(ResolvedExpression::BinaryOp{span, left: Box::new(left), operator, right: Box::new(right)})
                    }
                }
            }

            Expression::MemberAccess{ span, target, member} => {
                Ok(ResolvedExpression::MemberAccess{span, 
                    target: Box::new(self.resolve_expression(*target, scope)?), 
                    member: self.get_symbol(scope, member.clone()).expect(format!("Member not found: {:?}", member).as_str()).clone()
                })
            }

        }
    }

    pub fn resolve_shape_expression(&mut self, expression:ShapeExpression, scope:ScopeId) -> Result<ResolvedShapeExpression, CompileError> {
        match expression{
            ShapeExpression::Shape(k) => Ok(ResolvedShapeExpression::Simple(
                self.get_shape(scope, k.clone()).expect(format!("Shape not found in scope: {:?}", k).as_str()).clone()
            )),
            ShapeExpression::Primitive(kind) => Ok(ResolvedShapeExpression::Primitive(kind)),

            ShapeExpression::Applied { base, args } => {
                let base = self.get_shape(scope, base.clone())
                    .expect(format!("Shape not found in scope: {:?}", base).as_str()).clone();

                let mut resolved = Vec::new();
                for arg in args{
                    resolved.push(self.resolve_shape_expression(arg, scope)?);
                }
                Ok(ResolvedShapeExpression::Applied{ base, args: resolved})
            }
        }
    }

    pub fn resolve_mapping(&mut self, span:Span, mapping:Mapping, scope:ScopeId) -> Result<ResolvedMapping, CompileError> {
        let from = if let Some(symbol) = self.get_symbol(scope, mapping.from_slot.clone()){
            symbol.clone()
        } else { return Err(CompileError::UndefinedSymbol { span, name: mapping.from_slot }); };
        
        let to = if let Some(symbol) = self.get_symbol(scope, mapping.to_slot.clone()){
            symbol.clone()
        } else { return Err(CompileError::UndefinedSymbol { span, name: mapping.to_slot }); };

        Ok(ResolvedMapping{from, to})
    }

    pub fn resolve_statement(&mut self, statement:Statement, scope:ScopeId) -> Result<ResolvedStatement,CompileError> {
        match statement {
            Statement::Using { span, package } => {
                if let Ok(source) = fs::read_to_string(format!("/workspaces/SlotBox/src/{}.az", package)) {
                    let tokens = tokenizer::tokenize(&source);

                    match parser::parse(tokens){
                        Ok(ast) => Ok(ResolvedStatement::Using{ span, ast: self.analyze(ast)? }),
                        Err(error) => Err(CompileError::IoError { span, message: format!("Could not parse {}.az: {}", package, error) }),
                    }
                }
                else { Err(CompileError::IoError { span, message: format!("Could not find package {}.az", package) }) }
            }
            Statement::DeclareShape { span, name, slot_ids, mappings, generics } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }
    
                let symbol = self.declare_shape(span.clone(), scope, name, slot_ids, mappings, generics)?;
                Ok(ResolvedStatement::DeclareShape{ span, symbol:  symbol.clone() })
            }
            Statement::DeclareObject { span, name } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }
    
                let symbol = self.declare_object(scope, name);
                Ok(ResolvedStatement::DeclareObject{ span, symbol: symbol.clone() })
            }
            Statement::Attach { span, object, shape } => 
                Ok(ResolvedStatement::Attach{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape: self.resolve_shape_expression(shape, scope)?,
                }),
            Statement::Detach { span, object, shape } => 
                Ok(ResolvedStatement::Detach{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape: self.resolve_shape_expression(shape, scope)?
                }),
            Statement::AddMapping { span, object, mapping } => 
                Ok(ResolvedStatement::AddMapping{ span:span.clone(), 
                    object: self.resolve_expression(object, scope)?, 
                    mapping: self.resolve_mapping(span, mapping, scope)?,
                }),
            Statement::AttachWithRemap { span, object, shape, mappings } => {
                let mut resolved_mappings = Vec::new();
                for mapping in mappings {
                    resolved_mappings.push(self.resolve_mapping(span.clone(), mapping, scope)?);
                }
                Ok(ResolvedStatement::AttachWithRemap{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape: self.resolve_shape_expression(shape, scope)?, 
                    mappings: resolved_mappings,
                })
            },
            Statement::Print { span, expr } => Ok(ResolvedStatement::Print{span, expr: self.resolve_expression(expr, scope)?}),
            Statement::If { span, condition, true_statement, else_statement } => {
                let resolved_condition = self.resolve_expression(condition, scope)?;
                if resolved_condition.kind() != ValueKind::Bool {
                    return Err(CompileError::ExpectedBoolCondition { span, found: resolved_condition.kind() })
                }

                let resolved_true = self.resolve_statement(*true_statement, scope)?;
                let resolved_else = if let Some(statement) = *else_statement {
                    Some(self.resolve_statement(statement, scope)?)
                } else { None };

                Ok(ResolvedStatement::If{span, condition: resolved_condition, true_statement: Box::new(resolved_true), else_statement: Box::new(resolved_else)})
            }
            Statement::While { span, condition, statement } => {
                let resolved_condition = self.resolve_expression(condition, scope)?;
                if resolved_condition.kind() != ValueKind::Bool {
                    return Err(CompileError::ExpectedBoolCondition { span, found: resolved_condition.kind() })
                }

                let resolved_statement = self.resolve_statement(*statement, scope)?;
                Ok(ResolvedStatement::While{span, condition: resolved_condition, statement: Box::new(resolved_statement)})
            }
            
            Statement::For { span, local, statement } => todo!(),

            Statement::Assign {span, target, value } => {
                let target = self.resolve_expression(target, scope)?;
                let value = self.resolve_expression(value, scope)?;

                if target.kind() != value.kind() {
                    return Err(CompileError::TypeMismatch { span, expected: target.kind(), found: value.kind() })
                }

                Ok(ResolvedStatement::Assign{ span, target, value })
            }
            
            Statement::AssignAugmented {span, target, value, operator } => {
                let target = self.resolve_expression(target, scope)?;
                let value = self.resolve_expression(value, scope)?;

                // Ensure int types
                if target.kind() != ValueKind::Int32 || value.kind() != ValueKind::Int32 {
                    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Int32, found: target.kind() })
                }

                let expr = ResolvedExpression::BinaryOp {span:span.clone(), left: Box::new(target.clone()), operator, right: Box::new(value) };
                Ok(ResolvedStatement::Assign { span, target, value: expr })
            }
                
            Statement::Block(statements) => {
                let new_scope = self.create_scope(scope);

                let mut resolved_statements = Vec::new();
                for statement in statements {
                    resolved_statements.push(self.resolve_statement(statement, new_scope)?);
                }
                Ok(ResolvedStatement::Block(resolved_statements))
            }

            Statement::Break{span} => Ok(ResolvedStatement::Break{span}),
            Statement::Continue{span} => Ok(ResolvedStatement::Continue{span}),
            Statement::Return{span, value} => Ok(ResolvedStatement::Return{span, value:self.resolve_expression(value, scope)?}),
            Statement::Throw{span, message} => {
                let message = self.resolve_expression(message, scope)?;
                if message.kind() != ValueKind::String {
                    return Err(CompileError::InvalidThrow { span, found: message.kind() })
                }
                Ok(ResolvedStatement::Throw{span, message})
            }
        }
    }

    fn analyze(&mut self, statements: Vec<Statement>) -> Result<Vec<ResolvedStatement>, CompileError> {
        let mut resolved = Vec::new();

        for statement in statements {
            resolved.push(self.resolve_statement(statement, 0)?);
        }
    
        println!("\n Resolved Ast: \n{:?}", resolved);
        Ok(resolved)
    }
}

pub fn analyze(statements: Vec<Statement>) -> Result<Vec<ResolvedStatement>, CompileError> {
    let mut analyzer = Analyzer::new();
    analyzer.analyze(statements)
}

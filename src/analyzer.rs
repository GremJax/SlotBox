use std::{collections::HashMap, fs};

use crate::{AzimuthId, FunctionInfo, ObjectId, PrimitiveValue, ShapeId, Value, ValueKind, analyzer, executor::{self, OBJECT_INSTANCE, ShapeInstance}, parser::{self, Mapping}, tokenizer::{self, Span}}; 
use parser::{RawAzimuth, Expression, Statement, ShapeExpression};
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

pub type LocalId = u32;
type Identifier = String;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ShapeInfo {
    pub id: ShapeId,
    pub name: Identifier,
    pub static_id: Box<Option<Symbol>>,
    pub azimuths: Vec<AzimuthId>,
    pub generics: Vec<ResolvedShapeExpression>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AzimuthInfo {
    pub id: AzimuthId,
    pub name: Identifier,
    pub is_static: bool,
    pub shape_id: ShapeId,
    pub default_value: Box<Option<ResolvedExpression>>,
    pub value_type: ValueKind,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ObjectInfo {
    pub id: ObjectId,
    pub name: Identifier,
    pub known_shapes: Vec<ValueKind>
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalInfo {
    pub id: LocalId,
    pub name: Identifier,
    pub known_shapes: Vec<ValueKind>
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Symbol {
    Shape(ShapeInfo),
    Object(ObjectInfo),
    Azimuth(AzimuthInfo),
    Local(LocalInfo),
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
    pub locals: u32,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResolvedMapping {
    pub from: Symbol,
    pub to: Symbol,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedStatement {
    Using { span: Span, ast: Vec<ResolvedStatement>, },
    DeclareShape { span: Span, symbol: Symbol, azimuths: Vec<Symbol> },
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedShapeExpression {
    Simple(Symbol),
    Parameter(Identifier), // T
    Primitive(ValueKind),
    Array(Box<ResolvedShapeExpression>),
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
            ResolvedShapeExpression::Array(expr) => ValueKind::Array(Box::new(expr.kind())),
            other => todo!()
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedExpression {
    Value(Span, Value),
    Array(Span, Vec<ResolvedExpression>, ValueKind),
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
    },
    ArrayAccess  {
        span: Span,
        target: Box<ResolvedExpression>,
        index: Box<ResolvedExpression>,
    },
    FunctionCall  {
        span: Span,
        target: Box<ResolvedExpression>,
        args: Vec<ResolvedExpression>,
    },
    Function {
        span: Span,
        has_self: bool,
        input_types: Vec<ResolvedShapeExpression>,
        output_type: ResolvedShapeExpression,
        func: Box<ResolvedStatement>
    }
}

impl ResolvedExpression {
    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedExpression::Value(_, value) => value.kind(),
            ResolvedExpression::Array(_, _, value_type) => ValueKind::Array(Box::new(value_type.clone())),
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
        let global = Scope{id: 0, parent: None, symbols: HashMap::new(), locals:0};
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
        let scope = Scope{id: id.clone(), parent: Some(parent), symbols: HashMap::new(), locals:0};

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
            Some(Symbol::Local(info)) => symbol,
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
    
    fn declare_shape(&mut self, span: Span, scope_id: ScopeId, name: Identifier, slot_ids: Vec<RawAzimuth>, mappings: Vec<Mapping>, generics: Vec<ShapeExpression>) 
    -> Result<(&Symbol, Vec<&Symbol>), CompileError> {
        let id = self.next_shape_id.clone();
        self.next_shape_id += 1;
        
        // Shape symbol header
        let symbol = Symbol::Shape(ShapeInfo{
            id: id,
            name: name.clone(),
            static_id: Box::new(None), //static_info),
            azimuths: Vec::new(), //az_symbols,
            generics: Vec::new(),//resolved_generics,
        });

        // Azimuths
        let mut az_symbols = Vec::new();
        let mut az_ids = Vec::new();
        let mut az_id = self.next_azimuth_id.clone();
        let mut has_static = false;

        for azimuth in &slot_ids {
            // Azimuth headers
            let symbol = Symbol::Azimuth(AzimuthInfo {
                id: az_id,
                name: azimuth.name.clone(),
                shape_id: id,
                default_value: Box::new(None),//Box::new(default_value),
                is_static: azimuth.is_static,
                value_type: ValueKind::None,//self.resolve_shape_expression(azimuth.value_type, scope)?.kind(),
            });
            if azimuth.is_static { has_static = true; }
            az_ids.push(az_id);
            az_symbols.push(symbol);
            az_id += 1;
        }
        self.next_azimuth_id = az_id;
        
        {
            let scope = self.get_scope_mut(scope_id);

            // Insert azimuth headers
            for az_symbol in &az_symbols {
                match &az_symbol {
                    Symbol::Azimuth(info) => { scope.symbols.insert(info.name.clone(), az_symbol.clone()); }
                    _ => { return Err(CompileError::Error{span, message:format!("WHAT")}) }
                }
            }

            // Insert shape header
            scope.symbols.insert(name.clone(), symbol);
        }

        // Generics
        let mut resolved_generics = Vec::new();
        for generic in generics {
            resolved_generics.push(self.resolve_shape_expression(generic, scope_id)?);
        }

        // Static singleton
        let static_info = if has_static {
            Some(self.declare_object(scope_id, format!("{}::Static", name), ResolvedShapeExpression::Primitive(ValueKind::Shape(OBJECT_INSTANCE))).clone())
        } else { None };

        // Propegate Shape
        {
            let scope = self.get_scope_mut(scope_id);
            match scope.symbols.get_mut(&name).unwrap() {
                Symbol::Shape(info) => {
                    info.azimuths = az_ids;
                    info.generics = resolved_generics;
                    info.static_id = Box::new(static_info);
                }
                _ => todo!(),
            }
        }

        // Propegate Azimuths
        let mut names = Vec::new();
        for azimuth in slot_ids {
            names.push(azimuth.name.clone());

            let default_value = match azimuth.set_value {
                Some(expr) => Some(self.resolve_expression(expr, scope_id)?),
                _ => None,
            };

            let value_type = self.resolve_shape_expression(azimuth.value_type, scope_id)?;

            let scope = self.get_scope_mut(scope_id);
            let symbol = scope.symbols.get_mut(&azimuth.name).unwrap();
            match symbol {
                Symbol::Azimuth(azimuth_info) => {
                    azimuth_info.value_type = value_type.kind();
                    azimuth_info.default_value = Box::new(default_value);
                }
                _ => todo!(),
            }
        }
        
        // Retrieve propegated symbols
        let mut propegated_az = Vec::new();
        for name in names {
            propegated_az.push(self.get_symbol(scope_id, name).unwrap())
        }

        Ok((self.get_symbol(scope_id, name).unwrap(), propegated_az))
    }

    fn declare_object(&mut self, scope: ScopeId, name: Identifier, shape: ResolvedShapeExpression) -> &Symbol {
        let id = self.next_object_id.clone();
        self.next_object_id += 1;

        let mut known_shapes = Vec::new();
        known_shapes.push(shape.kind());

        let symbol = Symbol::Object(ObjectInfo{id: id, name: name.clone(), known_shapes});
        
        let scope = self.get_scope_mut(scope);
        scope.symbols.insert(name.clone(), symbol);
        scope.symbols.get(&name).unwrap()
    }

    fn declare_local(&mut self, scope: ScopeId, name: Identifier, shape: ResolvedShapeExpression) -> &Symbol {
        let mut known_shapes = Vec::new();
        known_shapes.push(shape.kind());
        
        let scope = self.get_scope_mut(scope);

        let id = scope.locals;
        scope.locals += 1;
        let symbol = Symbol::Local(LocalInfo{id: id, name: name.clone(), known_shapes});

        scope.symbols.insert(name.clone(), symbol);
        scope.symbols.get(&name).unwrap()
    }

    pub fn resolve_expression(&mut self, expression:Expression, scope:ScopeId) -> Result<ResolvedExpression, CompileError> {
        match expression {
            Expression::Value(span, value) => Ok(ResolvedExpression::Value(span, value)),
            Expression::Array(span, expressions, kind) => {
                let mut values = Vec::new();
                for item in expressions {
                    values.push(self.resolve_expression(item, scope.clone())?);
                }

                let kind = match kind {
                    Some(k) => k,
                    None => match values.first() {
                        Some(val) => val.kind(),
                        None => ValueKind::None,
                    }
                };

                Ok(ResolvedExpression::Array(span, values, kind))
            },
            
            Expression::Variable(span, k) => {
                match self.get_symbol(scope, k.clone()) {
                    Some(Symbol::Object(info)) => Ok(ResolvedExpression::Variable(span, Symbol::Object(info.clone()))),
                    Some(Symbol::Local(info)) => Ok(ResolvedExpression::Variable(span, Symbol::Local(info.clone()))),
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
                let target = self.resolve_expression(*target, scope)?;
                if let Some(member) = self.get_symbol(scope, member.clone()) {
                    Ok(ResolvedExpression::MemberAccess{span, 
                        target:Box::new(target),
                        member:member.clone()
                        })
                } else {
                    Err(CompileError::UndefinedSymbol { span, name: member })
                }
            }

            Expression::ArrayAccess{ span, target, index} => {
                let target = self.resolve_expression(*target, scope)?;
                let index = self.resolve_expression(*index, scope)?;

                if !index.kind().is_assignable_from(ValueKind::Int32) {
                    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Int32, found: index.kind() });
                }

                Ok(ResolvedExpression::ArrayAccess{span, target:Box::new(target), index:Box::new(index)})
            }

            Expression::FunctionCall { span, caller, target, args } => {
                let target = self.resolve_expression(*target, scope)?;
                let caller = self.resolve_expression(*caller, scope)?;

                let func = match target.kind() {
                    ValueKind::Function(func) => *func,
                    other => return Err(CompileError::Error { span, message:format!("Expected function, got {:?}", other) })
                };

                let mut resolved_args = Vec::new();

                // Add self
                if func.has_self {
                    resolved_args.push(caller);
                }

                for index in 0..args.len() {
                    let arg = self.resolve_expression(args.get(index).unwrap().clone(), scope)?;

                    match func.input_types.get(if func.has_self { index + 1 } else { index }) {
                        Some(param) => {
                            // Type check
                            if !arg.kind().is_assignable_from(param.clone()) {
                                return Err(CompileError::TypeMismatch { span, expected: param.clone(), found: arg.kind()})
                            }

                            // Accept arg
                            resolved_args.push(arg);
                        }
                        None => return Err(CompileError::Error { span, message:format!("Too few args: {}, needed: {}", args.len(), func.input_types.len()) }),
                    }
                }
                if resolved_args.len() < func.input_types.len() {
                    return Err(CompileError::Error { span, message:format!("Too few args: {}, needed: {}", args.len(), func.input_types.len()) })
                }

                Ok(ResolvedExpression::FunctionCall{span, target:Box::new(target), args:resolved_args})
            },

            Expression::Function {span, has_self, input_types, output_type, func } => {

                let new_scope = self.create_scope(scope);

                let mut resolved_inputs = Vec::new();
                for input in input_types {

                    let value_type = self.resolve_shape_expression(input.value_type, scope)?;
                    self.declare_local(new_scope, input.identifier, value_type.clone());

                    resolved_inputs.push(value_type);
                }

                Ok(ResolvedExpression::Function{span, has_self, 
                    input_types:resolved_inputs, 
                    output_type:self.resolve_shape_expression(output_type, new_scope)?, 
                    func:Box::new(self.resolve_statement(*func, new_scope)?)})
            }

        }
    }

    pub fn resolve_shape_expression(&mut self, expression:ShapeExpression, scope:ScopeId) -> Result<ResolvedShapeExpression, CompileError> {
        match expression{
            ShapeExpression::Shape(k) => Ok(ResolvedShapeExpression::Simple(
                self.get_shape(scope, k.clone()).expect(format!("Shape not found in scope: {:?}", k).as_str()).clone()
            )),
            ShapeExpression::Primitive(kind) => Ok(ResolvedShapeExpression::Primitive(kind)),
            ShapeExpression::Array(expr) => Ok(ResolvedShapeExpression::Array(Box::new(self.resolve_shape_expression(*expr, scope)?))),
            ShapeExpression::Applied { base, args } => {
                let base = self.get_shape(scope, base.clone())
                    .expect(format!("Shape not found in scope: {:?}", base).as_str()).clone();

                let mut resolved = Vec::new();
                for arg in args{
                    resolved.push(self.resolve_shape_expression(arg, scope)?);
                }
                Ok(ResolvedShapeExpression::Applied{ base, args: resolved})
            },
            ShapeExpression::Function { func } => {
                let output_type = self.resolve_shape_expression(func.output_type, scope)?.kind();

                let mut input_types = Vec::new();
                for input in func.input_types {
                    input_types.push(self.resolve_shape_expression(input.value_type, scope)?.kind())
                }

                let info = FunctionInfo{id: 0, input_types, output_type, has_self: func.has_self };

                Ok(ResolvedShapeExpression::Primitive(ValueKind::Function(Box::new(info))))
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
    
                let (symbol, az_symbols) = self.declare_shape(span.clone(), scope, name, slot_ids, mappings, generics)?;

                let mut azimuths = Vec::new();
                for azimuth in az_symbols {
                    azimuths.push(azimuth.clone());
                }

                Ok(ResolvedStatement::DeclareShape{ span, symbol: symbol.clone(), azimuths})
            }
            Statement::DeclareObject { span, name, shape } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }

                let shape = self.resolve_shape_expression(shape, scope)?;
    
                let symbol = self.declare_object(scope, name, shape);
                Ok(ResolvedStatement::DeclareObject{ span, symbol: symbol.clone() })
            }
            Statement::Attach { span, object, shape } => {
                let shape = self.resolve_shape_expression(shape, scope)?;

                Ok(ResolvedStatement::Attach{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape,
                })
            }
            Statement::Detach { span, object, shape } => {
                let shape = self.resolve_shape_expression(shape, scope)?;

                Ok(ResolvedStatement::Detach{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape,
                })
            }
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

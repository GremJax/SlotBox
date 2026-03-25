use std::{collections::HashMap, fs};

use crate::{AzimuthFlags, AzimuthId, FunctionSignature, GenericId, Number, ObjectId, ShapeId, Value, ValueKind, analyzer, executor::{self, OBJECT_INSTANCE, ShapeInstance}, intrinsic::IntrinsicOp, lexer::{self, Span}, loader::{Loader, Namespace, NamespaceId, NamespaceKind}, parser::{self, FunctionBody, Mapping}}; 
use parser::{RawAzimuth, Expression, Statement, ShapeExpression};
use lexer::{Operator};

#[derive(Debug, Clone)]
pub enum CompileError {
    Error { span: Span, message: String },
    UndefinedSymbol { span: Span, name: String },
    UndefinedStatic { span: Span, name: String },
    DuplicateSymbol { span: Span, name: String },
    ExpectedBoolCondition { span: Span, found: ValueKind },
    TypeMismatch { span: Span, expected: ValueKind, found: ValueKind, loc: String },
    IncorrectReturn { span: Span, expected: ValueKind, found: ValueKind },
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

            CompileError::TypeMismatch { span, expected, found, loc } =>
                write!(f, "{}: Type mismatch: expected {:?} for {}, got {:?}", span, expected, loc, found),
                
            CompileError::IncorrectReturn { span, expected, found } =>
                write!(f, "{}: Return type mismatch: expected {:?}, got {:?}", span, expected, found),

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
    pub static_id: Option<Box<ObjectInfo>>,
    pub azimuths: Vec<AzimuthId>,
    pub generics: Vec<ResolvedShapeExpression>,
    pub parent_ids: Vec<u32>,
    pub mappings: Vec<ResolvedMapping>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AzimuthInfo {
    pub id: AzimuthId,
    pub name: Identifier,
    pub flags: AzimuthFlags,
    pub shape_id: ShapeId,
    pub default_value: Option<Box<ResolvedExpression>>,
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
pub struct GenericInfo {
    pub id: GenericId,
    pub name: Identifier,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NamespaceIdentifier {
    pub id: NamespaceId,
}


#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Symbol {
    Object(ObjectInfo),
    Generic(GenericInfo),
    //Azimuth(AzimuthInfo),
    //Shape(ShapeInfo),
    Local(LocalInfo),
}

impl Symbol {
    fn get_name(&self) -> String {
        match self {
            //Symbol::Shape(info) => info.name.clone(),
            Symbol::Object(info) => info.name.clone(),
            //Symbol::Azimuth(info) => info.name.clone(),
            Symbol::Generic(info) => info.name.clone(),
            Symbol::Local(info) => info.name.clone(),
        }
    }
    
    fn kind(&self) -> ValueKind {
        match self {
            //Symbol::Shape(info) => ValueKind::Shape(ShapeInstance{id: info.id, generics: Vec::new()}),
            Symbol::Object(info) => ValueKind::Shape(executor::OBJECT_INSTANCE),
            //Symbol::Azimuth(info) => info.value_type.clone(),
            Symbol::Generic(info) => ValueKind::Generic(info.id),
            Symbol::Local(info) => info.known_shapes.first().unwrap().clone(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum SymbolId {
    Identifier(String),
    Azimuth {
        name: String,
        clarifier: Option<String>,
    },
    Shape{
        name: String,
        namespace: String,
    },
}

type ScopeId = u32;

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub symbols: HashMap<Identifier, Symbol>,
    pub generics: GenericId,
    pub locals: u32,
    pub using: Vec<NamespaceId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResolvedMapping {
    pub from: AzimuthInfo,
    pub to: AzimuthInfo,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedFunctionBody {
    Script(ResolvedStatement),
    Intrinsic(IntrinsicOp),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResolvedFunctionParameter {
    pub shape: ResolvedShapeExpression,
    pub local: LocalId
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedStatement {
    Using { span: Span, package: NamespaceId },
    Namespace { span: Span, name: Identifier, content: Vec<ResolvedStatement>},
    DeclareShape { span: Span, info: ShapeInfo, azimuths: Vec<AzimuthInfo> },
    DeclareObject { span: Span, local: LocalId, info: ObjectInfo, shape: ResolvedShapeExpression },
    DeclareLocal { span: Span, info: LocalInfo, value: ResolvedExpression },
    Detach { span: Span, object: ResolvedExpression, shape: ResolvedShapeExpression },
    AddMapping { span: Span, object: ResolvedExpression, mapping: ResolvedMapping },
    Attach { span: Span, object: ResolvedExpression, shape: ResolvedShapeExpression, mappings: Vec<ResolvedMapping>, },
    Print { span: Span, expr: ResolvedExpression },
    Expression { span: Span, expr: ResolvedExpression },
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
        span: Span, 
        local: LocalId,
        target: ResolvedExpression,
        statement: Box<ResolvedStatement>,
    },
    ForInc {
        span: Span, 
        local: LocalId,
        start: ResolvedExpression,
        cond: ResolvedExpression,
        inc: Box<ResolvedStatement>,
        statement: Box<ResolvedStatement>,
    },
    Try {
        span: Span, 
        try_statement: Box<ResolvedStatement>,
        catch_statement: Box<Option<ResolvedStatement>>,
    },
    Assign { span: Span, target: ResolvedExpression, value: ResolvedExpression },
    Seal { span: Span, target: ResolvedExpression, },
    Block(Vec<ResolvedStatement>),
    Break{ span: Span, }, 
    Continue{ span: Span, }, 
    Return{span: Span, value: ResolvedExpression },
    Throw{span: Span, message: ResolvedExpression },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ReturnStatus {
    Always,
    Conditionally,
    Never,
}

impl ResolvedStatement {

    pub fn walk_returns(&self, expected:ValueKind) -> Result<ReturnStatus, CompileError> {
        match self {
            ResolvedStatement::Return{span, value} => {
                if !value.kind().is_assignable_from(expected.clone()) { 
                    Err(CompileError::IncorrectReturn{span:span.clone(), expected, found: value.kind()})
                } else {
                    Ok(ReturnStatus::Always)
                }
            }
            ResolvedStatement::Block(statements) => {
                let mut conditionally_flag = false;

                for statement in statements {
                    match statement.walk_returns(expected.clone())? {
                        ReturnStatus::Always => return Ok(ReturnStatus::Always),
                        ReturnStatus::Conditionally => conditionally_flag = true,
                        ReturnStatus::Never => continue,
                    }
                }

                if conditionally_flag {
                    Ok(ReturnStatus::Conditionally)
                } else {
                    Ok(ReturnStatus::Never)
                }
            }
            ResolvedStatement::If{true_statement, else_statement, ..} => {
                let true_status = (*true_statement).walk_returns(expected.clone())?;
                
                let else_status = if let Some(statement) = else_statement.as_ref() {
                    statement.walk_returns(expected)?
                } else { ReturnStatus::Never };

                if true_status == ReturnStatus::Always && true_status == else_status {
                    Ok(ReturnStatus::Always)
                } else if true_status == ReturnStatus::Never && true_status == else_status {
                    Ok(ReturnStatus::Never)
                } else {
                    Ok(ReturnStatus::Conditionally)
                }
            }
            ResolvedStatement::For {statement, ..} => statement.walk_returns(expected),
            ResolvedStatement::While {statement, ..} => statement.walk_returns(expected),
            _ => Ok(ReturnStatus::Never)
        }
    }

}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedShapeExpression {
    Simple(Span, ShapeInfo),
    Parameter(Span, Identifier), // T
    Primitive(Span, ValueKind),
    Array(Span, Box<ResolvedShapeExpression>),
    Applied {
        span: Span,
        base: ShapeInfo,
        args: Vec<ResolvedShapeExpression>,
    },
    Optional(Span, Box<ResolvedShapeExpression>),
}

impl ResolvedShapeExpression {
    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedShapeExpression::Simple(_, info) => ValueKind::Shape(ShapeInstance{id: info.id, generics: Vec::new()}),
            ResolvedShapeExpression::Parameter(_, _) => ValueKind::Generic(0),
            ResolvedShapeExpression::Applied{base: info, args, ..} => ValueKind::Shape(ShapeInstance{
                id: info.id,
                generics: args.iter().map(|expr| expr.kind()).collect()
            }),
            ResolvedShapeExpression::Primitive(_, kind) => kind.clone(),
            ResolvedShapeExpression::Array(_, expr) => ValueKind::Array(Box::new(expr.kind())),
            ResolvedShapeExpression::Optional(_, expr) => ValueKind::Option(Box::new(expr.kind())),
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedExpression {
    Value(Span, Value),
    Array(Span, Vec<ResolvedExpression>, ValueKind),
    StringFormat(Span, Vec<ResolvedExpression>),
    Variable(Span, Symbol),
    Option(Span, Box<Option<ResolvedExpression>>),
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
    Ternary {
        span: Span,
        condition: Box<ResolvedExpression>,
        true_expr: Box<ResolvedExpression>,
        else_expr: Box<ResolvedExpression>,
    },
    MemberAccess  {
        span: Span,
        target: Box<ResolvedExpression>,
        member: AzimuthInfo,
        optional: bool,
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
        input_types: Vec<ResolvedFunctionParameter>,
        output_type: ResolvedShapeExpression,
        func: Box<Option<ResolvedFunctionBody>>,
        captures: Vec<LocalId>,
    }
}

impl ResolvedExpression {
    pub fn get_name(&self) -> String {
        match self {
            ResolvedExpression::Variable(_, symbol) => symbol.get_name(),
            ResolvedExpression::MemberAccess { member, target, .. } => format!("{}.{}", target.get_name(), member.name.clone()),
            ResolvedExpression::ArrayAccess { target, index, .. } => format!("{}[{}]", target.get_name(), index.get_name()),
            other => format!("{:?}", other.kind()),
        }
    }

    pub fn is_assignable(&self) -> bool {
        match self {
            ResolvedExpression::Variable(_, symbol) => true,
            ResolvedExpression::MemberAccess { member, .. } => match member {
                info => !info.flags.is_const,
                _ => false
            },
            ResolvedExpression::ArrayAccess { target, .. } => true,
            _ => false
        }
    }

    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedExpression::Value(_, value) => value.kind(),
            ResolvedExpression::Array(_, _, value_type) => ValueKind::Array(Box::new(value_type.clone())),
            ResolvedExpression::StringFormat(_, _) => ValueKind::String,
            ResolvedExpression::Variable(_, symbol) => symbol.kind(),
            ResolvedExpression::Option(_, option) => match *option.clone() {
                Some(expr) => expr.kind(),
                None => ValueKind::None,
            }
            ResolvedExpression::UnaryOp { operator, .. } => operator.kind(),
            ResolvedExpression::BinaryOp { operator, .. } => operator.kind(),
            ResolvedExpression::Ternary { true_expr, .. } => true_expr.kind(),
            ResolvedExpression::MemberAccess { member, .. } => member.value_type.clone(),
            ResolvedExpression::ArrayAccess { target, .. } => match target.kind() {
                ValueKind::Array(value_type) => *value_type.clone(),
                _ => unreachable!()
            }
            ResolvedExpression::FunctionCall { target, .. } => match target.kind() {
                ValueKind::Function(info) => info.output_type.clone(),
                _ => unreachable!()
            }
            ResolvedExpression::Function { input_types, output_type, has_self, .. } => {
                let resolved_out = output_type.kind();
                let resolved_in = input_types.iter().map(|i| i.shape.kind()).collect();
                ValueKind::Function(Box::new(FunctionSignature{ input_types:resolved_in, output_type:resolved_out, has_self:*has_self }))
            }
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
    loader: Loader,
    file_scopes: HashMap<Identifier, ScopeId>,

    azimuths: HashMap<u32, AzimuthInfo>,
    shapes: HashMap<u32, ShapeInfo>,

    next_scope_id: u32,
    next_object_id: u32,
}

impl Analyzer {
    pub fn new(loader:Loader) -> Self {
        let global = Scope{id: 0, parent: None, symbols: HashMap::new(), locals:0, generics:0, using:Vec::new()};
        let mut analyzer = Analyzer{
            scopes: Vec::new(), 
            loader,
            next_scope_id: 1,
            next_object_id: 1,
            file_scopes: HashMap::new(),
            azimuths: HashMap::new(),
            shapes: HashMap::new(),
        };
        analyzer.scopes.push(global);
        analyzer
    }

    pub fn get_file_scope_id(&self, namespace_id: &NamespaceId) -> ScopeId {
        for namespace in namespace_id {
            match self.file_scopes.get(namespace) {
                Some(id) => return *id,
                None => continue,
            }
        }
        0
    }

    pub fn get_scope(&self, id:ScopeId) -> &Scope {
        self.scopes.get(id as usize).expect(format!("Scope not found: {:?}", id).as_str())
    }

    pub fn get_scope_mut(&mut self, id:ScopeId) -> &mut Scope {
        self.scopes.get_mut(id as usize).expect(format!("Scope not found: {:?}", id).as_str())
    }

    pub fn create_scope(&mut self, parent:ScopeId) -> ScopeId {
        let id = self.next_scope_id;
        let parent_scope = self.get_scope(parent);

        let scope = Scope{
            id: id.clone(), 
            parent: Some(parent), 
            symbols: HashMap::new(), 
            locals:parent_scope.locals, 
            generics:parent_scope.generics,
            using: Vec::new(),
        };

        self.next_scope_id += 1;
        self.scopes.push(scope);
        id
    }

    pub fn get_using(&self, id:ScopeId) -> Vec<NamespaceId> {
        let scope = self.get_scope(id);
        match scope.parent {
            Some(parent) => {
                let mut using = self.get_using(parent);
                using.append(&mut scope.using.clone());
                using
            }
            _ => Vec::new()
        }
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

    pub fn get_object(&self, id:ScopeId, identifier:Identifier) -> Option<&ObjectInfo> {
        let symbol = self.get_symbol(id, identifier.clone());
        match symbol {
            Some(Symbol::Object(info)) => Some(info),
            _ => None,
        }
    }

    pub fn get_local(&self, id:ScopeId, identifier:Identifier) -> Option<&LocalInfo> {
        let symbol = self.get_symbol(id, identifier.clone());
        match symbol {
            Some(Symbol::Local(info)) => Some(info),
            _ => None,
        }
    }

    pub fn get_shape(&self, id:ScopeId, identifier:Identifier) -> Option<&ShapeInfo> {
        let using = self.get_using(id);
        let namespaces = self.loader.get_namespaces_matching(identifier, using.clone());

        if namespaces.len() <= 0 { 
            println!("Couldn't find any namespaces: {:?}", using);
            return None 
        }
        if namespaces.len() > 1 {
            //return Err(CompileError::AmbiguousShape{span, namespaces})
        }
        let (namespace_id, namespace) = &namespaces[0];

        return self.shapes.get(&namespace.id);
    }

    pub fn get_azimuth(&self, id:ScopeId, identifier:Identifier) -> Option<&AzimuthInfo> {
        let using = self.get_using(id);
        let found = self.loader.get_azimuths(identifier, using);

        if found.len() <= 0 { return None }
        let azimuth = found[0].1.id;
        // if found.len() > 1 { return Err() }

        return self.azimuths.get(&azimuth);
    }

    pub fn get_azimuth_namespace(&self, id:ScopeId, namespace_id:NamespaceId) -> Option<&AzimuthInfo> {
        let using = self.get_using(id);
        //self.loader.get_azimuths(name, using)
        todo!()
    }
    /*
    fn declare_shape(&mut self, span: Span, scope_id: ScopeId, name: Identifier, slot_ids: Vec<RawAzimuth>, parents: Vec<Identifier>, mappings: Vec<Mapping>, generics: Vec<ShapeExpression>) 
    -> Result<(&ShapeInfo, Vec<&AzimuthInfo>), CompileError> {
        let id = self.next_shape_id.clone();
        self.next_shape_id += 1;

        let new_scope = self.create_scope(scope_id);
        
        // Shape symbol header
        let info = ShapeInfo{
            id: id,
            name: name.clone(),
            static_id: None,     //static_info,
            azimuths: Vec::new(), //az_symbols,
            generics: Vec::new(),//resolved_generics,
            parent_ids: Vec::new(), //az_symbols,
            mappings: Vec::new(),//resolved_generics,
        };

        // Azimuths
        let mut az_info = Vec::new();
        let mut az_ids = Vec::new();
        let mut az_id = self.next_azimuth_id.clone();
        let mut has_static = false;

        for azimuth in &slot_ids {
            // Azimuth headers
            let info = AzimuthInfo {
                id: az_id,
                name: azimuth.name.clone(),
                shape_id: id,
                default_value: None,//Box::new(default_value),
                flags: azimuth.flags.clone(),
                value_type: ValueKind::None,//self.resolve_shape_expression(azimuth.value_type, scope)?.kind(),
            };
            if azimuth.flags.is_static { has_static = true; }
            az_ids.push(az_id);
            az_info.push(info);
            az_id += 1;
        }
        self.next_azimuth_id = az_id;
        
        {
            // Insert into parent scope
            let scope = self.get_scope_mut(scope_id);

            // Insert azimuth headers
            for info in &az_info {
                scope.symbols.insert(info.name.clone(), Symbol::Azimuth(info.clone()));
            }

            // Insert shape header
            scope.symbols.insert(name.clone(), Symbol::Shape(info.clone()));
        }

        // Generics
        let mut resolved_generics = Vec::new();
        for generic in generics {
            self.declare_generic(&generic, new_scope);
            let resolved_generic = self.resolve_shape_expression(generic, new_scope)?;
            resolved_generics.push(resolved_generic);
        }

        // Inheritance
        let mut parent_ids = Vec::new();
        for parent in parents {
            match self.get_shape(new_scope, parent.clone()) {
                Some(info) => parent_ids.push(info.id),
                _ => return Err(CompileError::UndefinedSymbol { span, name:parent })
            }
        }

        let mut resolved_mappings = Vec::new();
        for mapping in mappings {
            resolved_mappings.push(self.resolve_mapping(span.clone(), mapping, new_scope)?);
        }

        // Static singleton
        let static_info = if has_static {
            Some(Symbol::Object(self.declare_object(
                new_scope, 
                format!("{}::Static", name), 
                ResolvedShapeExpression::Primitive(ValueKind::Shape(OBJECT_INSTANCE))
            ).0))
        } else { None };

        // Propegate Shape
        {
            let scope = self.get_scope_mut(scope_id);
            match scope.symbols.get_mut(&name).unwrap() {
                Symbol::Shape(info) => {
                    info.azimuths = az_ids;
                    info.generics = resolved_generics;
                    info.static_id = Box::new(static_info);
                    info.parent_ids = parent_ids;
                    info.mappings = resolved_mappings;
                }
                _ => todo!(),
            }
        }

        // Propegate Azimuths
        let mut names = Vec::new();
        for azimuth in slot_ids {
            names.push(azimuth.name.clone());

            let default_value = match azimuth.set_value {
                Some(expr) => Some(Box::new(self.resolve_expression(expr, new_scope)?)),
                _ => None,
            };

            let value_type = self.resolve_shape_expression(azimuth.value_type, new_scope)?;

            let scope = self.get_scope_mut(scope_id);
            let symbol = scope.symbols.get_mut(&azimuth.name).unwrap();
            match symbol {
                Symbol::Azimuth(azimuth_info) => {
                    azimuth_info.value_type = value_type.kind();
                    azimuth_info.default_value = default_value;
                }
                _ => todo!(),
            }
            self.azimuths.insert()
        }
        
        // Retrieve propegated symbols
        let mut propegated_az = Vec::new();
        for name in names {
            propegated_az.push(self.get_azimuth(new_scope, name).unwrap())
        }

        Ok((self.get_shape(new_scope, name).unwrap(), propegated_az))
    }*/

    fn declare_generic(&mut self, shape: &ShapeExpression, scope: ScopeId) {
        let identifier = match shape {
            ShapeExpression::Shape(_, identifier) => identifier,
            _ => todo!()
        };
        
        let scope = self.get_scope_mut(scope);
        let id = scope.generics;
        scope.generics += 1;

        let symbol = Symbol::Generic(GenericInfo{id, name:identifier.clone()});

        scope.symbols.insert(identifier.clone(), symbol);
    }

    fn declare_object(&mut self, scope: ScopeId, name: Identifier, shape: ResolvedShapeExpression) -> (ObjectInfo, LocalId) {
        let id = self.next_object_id.clone();
        self.next_object_id += 1;

        let mut known_shapes = Vec::new();
        known_shapes.push(shape.kind());

        let local = self.declare_local(scope, name.clone(), shape);

        (ObjectInfo{id: id, name: name, known_shapes:known_shapes.clone()}, local)
    }

    fn declare_local(&mut self, scope: ScopeId, name: Identifier, shape: ResolvedShapeExpression) -> LocalId {
        let mut known_shapes = Vec::new();
        known_shapes.push(shape.kind());
        
        let scope = self.get_scope_mut(scope);

        let id = scope.locals;
        scope.locals += 1;
        let symbol = Symbol::Local(LocalInfo{id: id, name: name.clone(), known_shapes});

        scope.symbols.insert(name.clone(), symbol);
        id + 1
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

            Expression::StringFormat(span, expressions) => {
                let mut resolved = Vec::new();
                for expr in expressions {
                    resolved.push(self.resolve_expression(expr, scope)?);
                }

                Ok(ResolvedExpression::StringFormat(span, resolved))
            }

            Expression::Option(span, option) => todo!(),
            
            Expression::Variable(span, k) => {
                match self.get_symbol(scope, k.clone()) {
                    Some(Symbol::Object(info)) => Ok(ResolvedExpression::Variable(span, Symbol::Object(info.clone()))),
                    Some(Symbol::Local(info)) => Ok(ResolvedExpression::Variable(span, Symbol::Local(info.clone()))),
                    _ => {
                        match self.get_shape(scope, k.clone()) {
                            Some(info) => { match &info.static_id {
                                Some(info) => Ok(ResolvedExpression::Variable(span, Symbol::Object(*info.clone()))),
                                None => Err(CompileError::Error { span, message: format!("Shape {} does not have static instance", k) })
                            }}
                            _ => Err(CompileError::UndefinedSymbol { span, name: k })
                        }
                    }
                }
            }

            Expression::UnaryOp { span, operator, operand } => {
                match self.resolve_expression(*operand, scope)? {
                    // Bool Optimization
                    ResolvedExpression::Value(span, Value::Bool(val)) => match operator {
                        Operator::Not => Ok(ResolvedExpression::Value(span, (!val).into())),
                        operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:val.into()}),
                    },

                    // Int Optimization
                    ResolvedExpression::Value(span, Value::Number(val)) => {
                        let val = val.to_i32();
                        match operator {
                            Operator::Inc => Ok(ResolvedExpression::Value(span, (val + 1).into())),
                            Operator::Dec => Ok(ResolvedExpression::Value(span, (val - 1).into())),
                            Operator::BWNot => Ok(ResolvedExpression::Value(span, (!val).into())),
                            Operator::Sub => Ok(ResolvedExpression::Value(span, (-val).into())),
                            operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:val.into()}),
                        }
                    },

                    // String Optimization
                    ResolvedExpression::Value(span, Value::String(val)) => match operator {
                        Operator::Len => Ok(ResolvedExpression::Value(span, (val.len() as i32).into())),
                        operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:val.into()}),
                    },

                    // Array Optimization
                    ResolvedExpression::Value(span, Value::Array(vec, kind)) => match operator {
                        Operator::Len => Ok(ResolvedExpression::Value(span, (vec.len() as i32).into())),
                        operator => Err(CompileError::InvalidUnaryOp { span, operator, operand:Value::Array(vec, kind)}),
                    },

                    // Default
                    operand => Ok(ResolvedExpression::UnaryOp{span, operator, operand: Box::new(operand)})
                }
            }

            Expression::BinaryOp { span, left, operator, right } => {
                match (self.resolve_expression(*left, scope)?, self.resolve_expression(*right, scope)?) {
                    // Bool Optimization
                    (ResolvedExpression::Value(span, Value::Bool(left)), 
                        ResolvedExpression::Value(_, Value::Bool(right))) => match operator {
                        Operator::Equal => Ok(ResolvedExpression::Value(span, (left == right).into())),
                        Operator::NEqual => Ok(ResolvedExpression::Value(span, (left != right).into())),
                        Operator::And => Ok(ResolvedExpression::Value(span, (left && right).into())),
                        Operator::Or => Ok(ResolvedExpression::Value(span, (left || right).into())),
                        other => Err(CompileError::InvalidBinaryOp { span, operator:other, left:left.into(), right:right.into() }),
                    },
                    
                    // Int Optimization
                    (ResolvedExpression::Value(span, Value::Number(left)), 
                        ResolvedExpression::Value(_, Value::Number(right))) => {
                        let left = left.to_i32();
                        let right: i32 = right.to_i32();
                        match operator {
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
                            Operator::RangeLT => {
                                if left == right { Ok(ResolvedExpression::Value(span, Value::Array(Vec::new(), ValueKind::Int32))) }
                                else { Ok(ResolvedExpression::Value(span, executor::create_range(left, right + if left > right {1} else {-1}))) }
                            }
                            operator => Err(CompileError::InvalidBinaryOp { span, operator, left:left.into(), right:right.into() }),
                        }
                    },

                    // DQuestion optimization
                    (ResolvedExpression::Value(span, Value::None), right) => match operator {
                        Operator::DQuestion => Ok(right),
                        _ => Ok(ResolvedExpression::BinaryOp{span:span.clone(), left: Box::new(ResolvedExpression::Value(span, Value::None)), operator, right: Box::new(right)})
                    },

                    // Default
                    (left, right) => {
                        Ok(ResolvedExpression::BinaryOp{span, left: Box::new(left), operator, right: Box::new(right)})
                    }
                }
            }

            Expression::Ternary { span, condition, true_expr, else_expr  } => {
                let condition = self.resolve_expression(*condition, scope)?;
                if condition.kind() != ValueKind::Bool {
                    return Err(CompileError::ExpectedBoolCondition { span, found: condition.kind() })
                }

                let true_expr = self.resolve_expression(*true_expr, scope)?;
                let else_expr = self.resolve_expression(*else_expr, scope)?;

                if true_expr.kind() != else_expr.kind() {
                    return Err(CompileError::TypeMismatch { span, expected: true_expr.kind(), found: else_expr.kind(), loc: format!("ternary operation") } )
                }

                Ok(ResolvedExpression::Ternary{span, condition:Box::new(condition), true_expr:Box::new(true_expr), else_expr:Box::new(else_expr)})
            }

            Expression::MemberAccess{ span, target, qualifier, member, optional} => {
                let target = self.resolve_expression(*target, scope)?;
                let qualifier = match qualifier {
                    Some(shape_expr) => Some(self.resolve_shape_expression(shape_expr, scope)?),
                    None => None
                };

                if let Some(member) = self.get_azimuth(scope, member.clone()) {

                    Ok(ResolvedExpression::MemberAccess{span, 
                        target:Box::new(target),
                        member:member.clone(),
                        optional
                    })

                } else {
                    Err(CompileError::UndefinedSymbol { span, name: member })
                }
            }

            Expression::ArrayAccess{ span, target, index} => {
                let target = self.resolve_expression(*target, scope)?;
                let index = self.resolve_expression(*index, scope)?;

                if !index.kind().is_assignable_from(ValueKind::Int32) {
                    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Int32, found: index.kind(), loc:format!("array index") });
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
                                return Err(CompileError::TypeMismatch { span, expected: param.clone(), found: arg.kind(), loc:format!("function param")})
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

            Expression::Function {span, has_self, input_types, output_type, func, captures } => {

                let new_scope = self.create_scope(scope);

                let mut resolved_inputs = Vec::new();
                for input in input_types {
                    let value_type = self.resolve_shape_expression(input.value_type, scope)?;
                    let id = self.declare_local(new_scope, input.identifier, value_type.clone());

                    let resolved_input = ResolvedFunctionParameter{ shape:value_type, local:id };

                    resolved_inputs.push(resolved_input);
                }

                let mut resolved_captures = Vec::new();
                for capture in captures {
                    let symbol = self.get_symbol(scope, capture);

                    let id = match symbol {
                        Some(Symbol::Local(info)) => info.id,
                        _ => todo!()
                    };
                    resolved_captures.push(id);
                }

                let output_type = self.resolve_shape_expression(output_type, new_scope)?;

                let body = match func.as_ref() {
                    None => None,
                    Some(FunctionBody::Script(func)) => {
                        let statement = self.resolve_statement(func.clone(), new_scope)?;
                        match statement.walk_returns(output_type.kind())? {
                            ReturnStatus::Always => {},
                            _ => if !output_type.kind().is_assignable_from(ValueKind::None) {
                                return Err(CompileError::IncorrectReturn { span, expected: output_type.kind(), found: ValueKind::None })
                            }
                        }
                        Some(ResolvedFunctionBody::Script(statement))
                    },
                    Some(FunctionBody::Intrinsic(func)) => Some(ResolvedFunctionBody::Intrinsic(*func)),
                };

                Ok(ResolvedExpression::Function{span, has_self, 
                    input_types:resolved_inputs, 
                    output_type, 
                    func:Box::new(body),
                    captures:resolved_captures
                })
            }

        }
    }

    pub fn resolve_shape_expression(&mut self, expression:ShapeExpression, scope:ScopeId) -> Result<ResolvedShapeExpression, CompileError> {
        match expression{
            ShapeExpression::Shape(span, k) => match self.get_shape(scope, k.clone()) {
                Some(info) => Ok(ResolvedShapeExpression::Simple(span, info.clone())),
                _ => { 
                    match self.get_symbol(scope, k.clone()) {
                        Some(Symbol::Generic(info)) => Ok(ResolvedShapeExpression::Parameter(span, info.name.clone())),
                        _ => Err(CompileError::UndefinedSymbol { span, name: format!("{} A", k) })
                    }
                }
            }
            ShapeExpression::Primitive(span, kind) => Ok(ResolvedShapeExpression::Primitive(span, kind)),
            ShapeExpression::Array(span, expr) => Ok(ResolvedShapeExpression::Array(span, Box::new(self.resolve_shape_expression(*expr, scope)?))),
            ShapeExpression::Optional(span, expr) => Ok(ResolvedShapeExpression::Optional(span, Box::new(self.resolve_shape_expression(*expr, scope)?))),
            ShapeExpression::Applied { span, base, args } => {
                let base = self.get_shape(scope, base.clone())
                    .expect(format!("Shape not found in scope: {:?}", base).as_str()).clone();

                let mut resolved = Vec::new();
                for arg in args{
                    resolved.push(self.resolve_shape_expression(arg, scope)?);
                }
                Ok(ResolvedShapeExpression::Applied{ span, base, args: resolved})
            },
            ShapeExpression::FunctionSignature(span, signature) => {
                let output_type = self.resolve_shape_expression(signature.output_type, scope)?.kind();

                let mut input_types = Vec::new();
                for input in signature.input_types {
                    input_types.push(self.resolve_shape_expression(input, scope)?.kind())
                }

                let info = FunctionSignature{input_types, output_type, has_self: signature.has_self };

                Ok(ResolvedShapeExpression::Primitive(span, ValueKind::Function(Box::new(info))))
            }
        }
    }

    pub fn resolve_mapping(&mut self, span:Span, mapping:Mapping, scope:ScopeId) -> Result<ResolvedMapping, CompileError> {
        let from = if let Some(symbol) = self.get_azimuth(scope, mapping.from_slot.clone()){
            symbol.clone()
        } else { return Err(CompileError::UndefinedSymbol { span, name: mapping.from_slot }); };
        
        let to = if let Some(symbol) = self.get_azimuth(scope, mapping.to_slot.clone()){
            symbol.clone()
        } else { return Err(CompileError::UndefinedSymbol { span, name: mapping.to_slot }); };

        Ok(ResolvedMapping{from, to})
    }

    pub fn get_azimuth_with_id(&self, scope:ScopeId, id:AzimuthId) -> Option<&AzimuthInfo> {
        self.azimuths.get(&id)
    }

    pub fn resolve_statement(&mut self, statement:Statement, scope:ScopeId) -> Result<ResolvedStatement,CompileError> {
        match statement {
            Statement::Expression {span, expr} => Ok(ResolvedStatement::Expression{span, expr:self.resolve_expression(expr, scope)?}),
            Statement::Using { span, package } => {
                self.get_scope_mut(scope).using.push(package.clone());
                Ok(ResolvedStatement::Using{ span, package })
            }
            Statement::Namespace{ span, name, content } => todo!(),
            Statement::DeclareAzimuth{ span, azimuth } => todo!(),
            Statement::DeclareShape { span, name, slot_ids, parents, mappings, generics } => {
                //let my_scope = self.get_scope_mut(scope);
                //if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }
    
                //let (info, az_symbols) = self.declare_shape(span.clone(), scope, name, slot_ids, parents, mappings, generics)?;

                //let mut azimuths = Vec::new();
                //for azimuth in az_symbols {
                //    azimuths.push(azimuth.clone());
                //}

                //Ok(ResolvedStatement::DeclareShape{ span, info: info.clone(), azimuths})
                Ok(ResolvedStatement::Block(Vec::new()))
            }
            Statement::DeclareObject { span, name, shape } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }

                let shape = self.resolve_shape_expression(shape, scope)?;
    
                let (info, local) = self.declare_object(scope, name, shape.clone());
                Ok(ResolvedStatement::DeclareObject{ span, local, info, shape })
            }
            Statement::DeclareLocal { span, name, value } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { return Err(CompileError::DuplicateSymbol{span, name}); }

                let value = self.resolve_expression(value, scope)?;
                let kind = ResolvedShapeExpression::Primitive(span.clone(), value.kind());
    
                let id = self.declare_local(scope, name.clone(), kind);
                let mut kinds = Vec::new();
                kinds.push(value.kind());
                let info = LocalInfo{ id, name, known_shapes: kinds };
                Ok(ResolvedStatement::DeclareLocal{ span, info, value })
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
            Statement::Attach { span, object, shape, mappings } => {
                let mut resolved_mappings = Vec::new();
                for mapping in mappings {
                    resolved_mappings.push(self.resolve_mapping(span.clone(), mapping, scope)?);
                }

                // Validate abstract
                let shape = self.resolve_shape_expression(shape, scope)?;
                let (azimuths, defaults) = match &shape {
                    ResolvedShapeExpression::Simple(span, info) => (info.azimuths.clone(), info.mappings.clone()),
                    _ => (Vec::new(), Vec::new()),
                };
                for id in azimuths {
                    // Find az with id
                    if let Some(found) = self.get_azimuth_with_id(scope, id) {
                        if found.flags.is_abstract {
                            match defaults.iter().find(|mapping| mapping.from.id == id) {
                                Some(_) => continue,
                                None => {}
                            }
                            match resolved_mappings.iter().find(|mapping| mapping.from.id == id) {
                                Some(_) => continue,
                                None => return Err(CompileError::Error { span, message:format!("Abstract attached without mapping") })
                            }
                        }
                    }
                }

                Ok(ResolvedStatement::Attach{ span, 
                    object: self.resolve_expression(object, scope)?, 
                    shape,
                    mappings: resolved_mappings,
                })
            },
            Statement::Print { span, expr } => Ok(ResolvedStatement::Print{span, expr: self.resolve_expression(expr, scope)?}),
            Statement::Seal { span, target } => {
                let target = self.resolve_expression(target, scope)?;
                match target.kind() {
                    ValueKind::Shape(_) => {},
                    _ => return Err(CompileError::Error { span, message: format!("{} ({:?}) is not sealable", target.get_name(), target.kind()) })
                }

                Ok(ResolvedStatement::Seal{span, target})
            }
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
            Statement::Try { span, try_statement, catch_statement } => {
                let resolved_try = self.resolve_statement(*try_statement, scope)?;

                let resolved_catch = if let Some(statement) = *catch_statement {
                    Some(self.resolve_statement(statement, scope)?)
                } else { None };

                Ok(ResolvedStatement::Try{span, try_statement:Box::new(resolved_try), catch_statement:Box::new(resolved_catch)})
            }
            Statement::For { span, local, target, statement } => {
                let target = self.resolve_expression(target, scope)?;

                let iterable_type = match target.kind() {
                    ValueKind::Array(kind) => ShapeExpression::Primitive(span.clone(), *kind),
                    other => return Err(CompileError::Error{span, message:format!("Expected array in for loop declaration, got {:?}",other)}),
                };

                let new_scope = self.create_scope(scope);
                
                let resolved_type = self.resolve_shape_expression(iterable_type, scope)?;
                let id = self.declare_local(new_scope, local, resolved_type.clone()).clone();

                let statement = self.resolve_statement(*statement, new_scope)?;

                Ok(ResolvedStatement::For{span, local:id, target, statement:Box::new(statement)})
            }
            
            Statement::ForInc { span, local, start, cond, inc, statement } => {
                let resolved_start = self.resolve_expression(start, scope)?;
                if !resolved_start.kind().is_assignable_from(ValueKind::Number) {
                    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Number, found: resolved_start.kind(), loc: format!("for loop start value") } )
                }
                let resolved_condition = self.resolve_expression(cond, scope)?;
                if !resolved_condition.kind().is_assignable_from(ValueKind::Bool) {
                    return Err(CompileError::ExpectedBoolCondition { span, found: resolved_condition.kind() })
                }

                let new_scope = self.create_scope(scope);
                
                let resolved_type = ResolvedShapeExpression::Primitive(span.clone(), ValueKind::Int32);
                let id = self.declare_local(new_scope, local, resolved_type.clone()).clone();

                let inc = self.resolve_statement(*inc, new_scope)?;
                let statement = self.resolve_statement(*statement, new_scope)?;

                Ok(ResolvedStatement::ForInc{span, local:id, start:resolved_start, cond:resolved_condition, inc:Box::new(inc), statement:Box::new(statement)})
            }

            Statement::Assign {span, target, value } => {
                let target = self.resolve_expression(target, scope)?;

                if !target.is_assignable() {
                    return Err(CompileError::Error { span, message:format!("{:?} is not assignable", target.get_name()) })
                }

                let value = self.resolve_expression(value, scope)?;

                if !target.kind().is_assignable_from(value.kind()) {
                    return Err(CompileError::TypeMismatch { span, expected: target.kind(), found: value.kind(), loc:format!("value assignment") })
                }

                Ok(ResolvedStatement::Assign{ span, target, value })
            }
            
            Statement::AssignAugmented {span, target, value, operator } => {
                let target = self.resolve_expression(target, scope)?;

                if !target.is_assignable() {
                    return Err(CompileError::Error { span, message:format!("{:?} is not assignable", target) })
                }

                let value = self.resolve_expression(value, scope)?;

                // Ensure int types
                //if !target.kind().is_assignable_from(ValueKind::Number) {
                //    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Int32, found: target.kind(), loc:format!("augmented assignment target") })
                //}
                //if !value.kind().is_assignable_from(ValueKind::Number) {
                //    return Err(CompileError::TypeMismatch { span, expected: ValueKind::Int32, found: value.kind(), loc:format!("augmented assignment value") })
                //}

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

    fn resolve_namespace_headers(&mut self, namespace: &Namespace, scope: ScopeId) -> Result<(), CompileError> {
        for azimuth in &namespace.azimuths {
            self.azimuths.insert(azimuth.id, AzimuthInfo { 
                id: azimuth.id, 
                name: azimuth.name.clone(), 
                flags: azimuth.flags.clone(), 
                shape_id: 0, 
                default_value: None, 
                value_type: ValueKind::None, 
            });
        }
        match namespace.kind {
            NamespaceKind::Shape{ .. } => {
                self.shapes.insert(namespace.id, ShapeInfo { 
                    id: namespace.id,
                    name: namespace.name.clone(),
                    static_id: None,
                    azimuths: namespace.azimuths.iter().map(|az| az.id).collect(),
                    generics: Vec::new(),
                    parent_ids: Vec::new(),
                    mappings: Vec::new(), 
                });
            }
            _ => {}
        }
        for child in namespace.children.values() {
            self.resolve_namespace_headers(child, scope)?;
        }
        Ok(())
    }

    fn resolve_namespace_kinds(&mut self, namespace: &Namespace, scope: ScopeId) -> Result<(), CompileError> {
        let span = namespace.span.clone();
        for azimuth in &namespace.azimuths {
            let resolved = self.resolve_shape_expression(azimuth.kind.clone(), scope)?;
            match self.azimuths.get_mut(&azimuth.id) {
                None => return Err(CompileError::Error{span, message: format!("Dead azimuth: {}", namespace.id)}),
                Some(info) => {
                    info.value_type = resolved.kind();
                }
            }
        }
        for child in namespace.children.values() {
            self.resolve_namespace_kinds(child, scope)?;
        }
        Ok(())
    }

    fn resolve_namespace_shapes(&mut self, namespace: &Namespace, scope: ScopeId) -> Result<(), CompileError> {
        let span = namespace.span.clone();
        match &namespace.kind {
            NamespaceKind::Shape{ generics, mappings, parents, has_static } => {
                
                // Generics
                let mut resolved_generics = Vec::new();
                for generic in generics {
                    self.declare_generic(generic, scope);
                    let resolved_generic = self.resolve_shape_expression(generic.clone(), scope)?;
                    resolved_generics.push(resolved_generic);
                }

                // Inheritance
                let mut parent_ids = Vec::new();
                for parent in parents {
                    match self.get_shape(scope, parent.clone()) {
                        Some(info) => parent_ids.push(info.id),
                        _ => return Err(CompileError::UndefinedSymbol { span, name:parent.clone() })
                    }
                }

                // Mappings
                let mut resolved_mappings = Vec::new();
                for mapping in mappings {
                    resolved_mappings.push(self.resolve_mapping(span.clone(), mapping.clone(), scope)?);
                }

                // Static singleton
                let static_info = if *has_static {
                    Some(Box::new(self.declare_object(
                        scope, 
                        format!("{}::Static", namespace.name), 
                        ResolvedShapeExpression::Primitive(span.clone(), ValueKind::Shape(OBJECT_INSTANCE))
                    ).0))
                } else { None };

                // Set info
                match self.shapes.get_mut(&namespace.id) {
                    None => return Err(CompileError::Error{span, message: format!("Dead azimuth: {}", namespace.id)}),
                    Some(info) => {
                        info.generics = resolved_generics;
                        info.mappings = resolved_mappings;
                        info.static_id = static_info;
                        info.parent_ids = parent_ids;
                    }
                }
            }
            _ => {}
        }
        for child in namespace.children.values() {
            self.resolve_namespace_shapes(child, scope)?;
        }
        Ok(())
    }

    fn resolve_namespace_defaults(&mut self, namespace: &Namespace, scope: ScopeId) -> Result<(), CompileError> {
        for azimuth in &namespace.azimuths {
            let resolved = match &azimuth.default_value {
                None => continue,
                Some(value) => self.resolve_expression(value.clone(), scope)?
            };

            match self.azimuths.get_mut(&namespace.id) {
                None => return Err(CompileError::Error{span:namespace.span.clone(), message: format!("Dead azimuth: {}", namespace.id)}),
                Some(info) => {
                    info.default_value = Some(Box::new(resolved));
                }
            }
        }
        for child in namespace.children.values() {
            self.resolve_namespace_defaults(child, scope)?;
        }
        Ok(())
    }

    fn analyze(&mut self) -> Result<Vec<ResolvedStatement>, CompileError> {
        let files = std::mem::take(&mut self.loader.files);
        let load_order = std::mem::take(&mut self.loader.load_order);
        let root_name = self.loader.root.name.clone();
        let root = self.loader.root.clone();

        // Resolve namespaces
        println!("Headers...");
        self.resolve_namespace_headers(&root, 0)?;
        println!("Done.\n{:?}\n{:?}\nKinds...", self.azimuths, self.shapes);
        self.resolve_namespace_kinds(&root, 0)?;
        println!("Done.\n{:?}\n{:?}\nShapes...", self.azimuths, self.shapes);
        self.resolve_namespace_shapes(&root, 0)?;
        println!("Done.\n{:?}\n{:?}\nDefaults...", self.azimuths, self.shapes);
        self.resolve_namespace_defaults(&root, 0)?;
        println!("Done.\n{:?}\n{:?}\nStatements...", self.azimuths, self.shapes);

        // Resolve statements
        let mut resolved = Vec::new();

        for (loc, namespace) in &load_order {
            let file = match files.get(&loc) {
                Some(file) => file,
                None => return Err(CompileError::Error { span:Span::new(0,0, loc.url.clone()), message: format!("Fucked up") }),
            };

            let new_scope = self.create_scope(0);
            self.get_scope_mut(new_scope).using.push([namespace.clone()].to_vec());

            for statement in file {
                resolved.push(self.resolve_statement(statement.clone(), new_scope)?);
            }
        }
    
        //println!("\n Resolved Ast: \n{:?}", resolved);
        Ok(resolved)
    }
}

pub fn analyze(loader: Loader) -> Result<Vec<ResolvedStatement>, CompileError> {
    let mut analyzer = Analyzer::new(loader);
    analyzer.analyze()
}

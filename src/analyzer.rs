use std::{collections::HashMap, fs};

use crate::{AzimuthId, ObjectId, PrimitiveValue, ShapeId, Value, ValueKind, analyzer, executor::{self, ShapeInstance}, parser::{self, Mapping}, tokenizer}; 
use parser::{Azimuth, Expression, Statement, ShapeExpression};
use tokenizer::{Operator};

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
    Using { ast: Vec<ResolvedStatement>, },
    DeclareShape { symbol: Symbol },
    DeclareObject { symbol: Symbol },
    Attach { object: ResolvedExpression, shape: ResolvedShapeExpression, },
    Detach { object: ResolvedExpression, shape: ResolvedShapeExpression },
    AddMapping { object: ResolvedExpression, mapping: ResolvedMapping },
    AttachWithRemap { object: ResolvedExpression, shape: ResolvedShapeExpression, mappings: Vec<ResolvedMapping>, },
    Print { expr: ResolvedExpression },
    If {
        condition: ResolvedExpression,
        true_statement: Box<ResolvedStatement>,
        else_statement: Box<ResolvedStatement>,
    },
    While {
        condition: ResolvedExpression,
        statement: Box<ResolvedStatement>,
    },
    For {

    },
    Assign {
        target: Box<ResolvedExpression>,
        value: Box<ResolvedExpression>,
    }
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
            other => panic!("Unexpected shape type: {:?}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedExpression {
    Literal(Value),
    Array(Vec<ResolvedExpression>),
    Variable(Symbol),
    UnaryOp {
        operator: Operator,
        operand: Box<ResolvedExpression>,
    },
    BinaryOp {
        left: Box<ResolvedExpression>,
        operator: Operator,
        right: Box<ResolvedExpression>,
    },
    MemberAccess  {
        target: Box<ResolvedExpression>,
        member: Symbol,
    }
}

impl ResolvedExpression {
    pub fn kind(&self) -> ValueKind {
        match self {
            ResolvedExpression::Literal(Value::Single(value)) => value.kind(),
            ResolvedExpression::Literal(Value::Array(values)) => todo!(),
            ResolvedExpression::Literal(Value::Empty) => ValueKind::None,
            ResolvedExpression::Array(resolved_expressions) => todo!(),
            ResolvedExpression::Variable(symbol) => symbol.kind(),
            ResolvedExpression::UnaryOp { operator, operand:_ } => operator.kind(),
            ResolvedExpression::BinaryOp {left:_, operator, right:_ } => operator.kind(),
            ResolvedExpression::MemberAccess {target:_, member } => member.kind(),
            _ => todo!()
        }
    }
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
    
    fn declare_shape(&mut self, scope: ScopeId, name: Identifier, slot_ids: Vec<Azimuth>, mappings: Vec<Mapping>, generics: Vec<ShapeExpression>) -> &Symbol {
        let id = self.next_shape_id.clone();
        self.next_shape_id += 1;

        // Azimuths
        let mut az_symbols = Vec::new();
        let mut az_id = self.next_azimuth_id.clone();
        let mut has_static = false;

        for azimuth in slot_ids {
            let default_value = match azimuth.set_value {
                Some(expr) => Some(self.resolve_expression(expr, scope)),
                _ => None,
            };

            let symbol = Symbol::Azimuth(AzimuthInfo {
                id: az_id,
                name: azimuth.name,
                shape_id: id,
                default_value: Box::new(default_value),
                is_static: azimuth.is_static,
                value_type: self.resolve_shape_expression(azimuth.value_type, scope).kind(),
            });
            if azimuth.is_static { has_static = true; }
            az_symbols.push(symbol);
            az_id += 1;
        }
        self.next_azimuth_id = az_id;

        // Generics
        let mut resolved_generics = Vec::new();
        for generic in generics {
            resolved_generics.push(self.resolve_shape_expression(generic, scope));
        }

        // Static singleton
        let static_info = if has_static {
            Some(self.declare_object(scope, format!("{}::Static", name)).clone())
        } else { None };
        
        let scope = self.get_scope_mut(scope);

        for az_symbol in &az_symbols {
            match &az_symbol {
                Symbol::Azimuth(info) => { scope.symbols.insert(info.name.clone(), az_symbol.clone()); }
                _ => { panic!("How tf"); }
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
        scope.symbols.get(&name).unwrap()
    }

    fn declare_object(&mut self, scope: ScopeId, name: Identifier) -> &Symbol {
        let id = self.next_object_id.clone();
        self.next_object_id += 1;

        let symbol = Symbol::Object(ObjectInfo{id: id, name: name.clone()});
        
        let scope = self.get_scope_mut(scope);
        scope.symbols.insert(name.clone(), symbol);
        scope.symbols.get(&name).unwrap()
    }

    pub fn resolve_expression(&self, expression:Expression, scope:ScopeId) -> ResolvedExpression {
        match expression {
            Expression::Literal(value) => ResolvedExpression::Literal(value),
            Expression::Array(expressions) => {
                let mut values = Vec::new();
                for item in expressions {
                    values.push(self.resolve_expression(item, scope.clone()));
                }
                ResolvedExpression::Array(values)
            },
            
            Expression::Variable(k) => {
                match self.get_symbol(scope, k.clone()) {
                    Some(Symbol::Object(info)) => ResolvedExpression::Variable(Symbol::Object(info.clone())),
                    Some(Symbol::Shape(info)) => {
                        match *info.static_id.clone() {
                            Some(symbol) => ResolvedExpression::Variable(symbol),
                            _ => panic!("Could not find static {} in scope", k),
                        }
                    }
                    _ => panic!("Could not find {} in scope", k),
                }
            }

            Expression::UnaryOp { operator, operand } => {
                match self.resolve_expression(*operand, scope) {
                    // Bool Optimization
                    ResolvedExpression::Literal(Value::Single(PrimitiveValue::Bool(val))) => match operator {
                        Operator::Not => ResolvedExpression::Literal((!val).into()),
                        other => panic!("Invalid unary operator on bool: {:?}{:?}", other, val),
                    },

                    // Int Optimization
                    ResolvedExpression::Literal(Value::Single(PrimitiveValue::Int32(val))) => match operator {
                        Operator::Inc => ResolvedExpression::Literal((val + 1).into()),
                        Operator::Dec => ResolvedExpression::Literal((val - 1).into()),
                        Operator::BWNot => ResolvedExpression::Literal((!val).into()),
                        other => panic!("Invalid unary operator on int32: {:?}{:?}", other, val),
                    },

                    // Default
                    operand => ResolvedExpression::UnaryOp{operator, operand: Box::new(operand)}
                }
            }

            Expression::BinaryOp { left, operator, right } => {
                match (self.resolve_expression(*left, scope), self.resolve_expression(*right, scope)) {
                    // Bool Optimization
                    (ResolvedExpression::Literal(Value::Single(PrimitiveValue::Bool(left))), 
                        ResolvedExpression::Literal(Value::Single(PrimitiveValue::Bool(right)))) => match operator {
                        Operator::Equal => ResolvedExpression::Literal((left == right).into()),
                        Operator::NEqual => ResolvedExpression::Literal((left != right).into()),
                        Operator::And => ResolvedExpression::Literal((left && right).into()),
                        Operator::Or => ResolvedExpression::Literal((left || right).into()),
                        other => panic!("Invalid binary operator on bools: {:?} {:?} {:?}", left, other, right),
                    },
                    
                    // Int Optimization
                    (ResolvedExpression::Literal(Value::Single(PrimitiveValue::Int32(left))), 
                        ResolvedExpression::Literal(Value::Single(PrimitiveValue::Int32(right)))) => match operator {
                        Operator::Equal => ResolvedExpression::Literal((left == right).into()),
                        Operator::NEqual => ResolvedExpression::Literal((left != right).into()),
                        Operator::LT => ResolvedExpression::Literal((left < right).into()),
                        Operator::GT => ResolvedExpression::Literal((left > right).into()),
                        Operator::LTE => ResolvedExpression::Literal((left <= right).into()),
                        Operator::GTE => ResolvedExpression::Literal((left >= right).into()),
                        Operator::Add => ResolvedExpression::Literal((left + right).into()),
                        Operator::Sub => ResolvedExpression::Literal((left - right).into()),
                        Operator::Mul => ResolvedExpression::Literal((left * right).into()),
                        Operator::Div => ResolvedExpression::Literal((left / right).into()),
                        Operator::Mod => ResolvedExpression::Literal((left % right).into()),
                        Operator::BWAnd => ResolvedExpression::Literal((left & right).into()),
                        Operator::BWOr => ResolvedExpression::Literal((left | right).into()),
                        Operator::BWXor => ResolvedExpression::Literal((left ^ right).into()),
                        Operator::BWShiftL => ResolvedExpression::Literal((left << right).into()),
                        Operator::BWShiftR => ResolvedExpression::Literal((left >> right).into()),
                        Operator::Range => ResolvedExpression::Literal(executor::create_range(left, right)),
                        Operator::RangeLT => ResolvedExpression::Literal(executor::create_range(left, right - 1)),
                        other => panic!("Invalid binary operator on ints: {:?} {:?} {:?}", left, other, right),
                    },

                    // Default
                    (left, right) => {


                        ResolvedExpression::BinaryOp{left: Box::new(left), operator, right: Box::new(right)}
                    }
                }
            }

            Expression::MemberAccess{ target, member} => 
                ResolvedExpression::MemberAccess{
                    target: Box::new(self.resolve_expression(*target, scope)), 
                    member: self.get_symbol(scope, member.clone()).expect(format!("Member not found: {:?}", member).as_str()).clone()
                },

        }
    }

    pub fn resolve_shape_expression(&mut self, expression:ShapeExpression, scope:ScopeId) -> ResolvedShapeExpression {
        match expression{
            ShapeExpression::Simple(k) => ResolvedShapeExpression::Simple(
                self.get_shape(scope, k.clone()).expect(format!("Shape not found in scope: {:?}", k).as_str()).clone()
            ),

            ShapeExpression::Parameter(k) => ResolvedShapeExpression::Parameter(k),
            ShapeExpression::Primitive(kind) => ResolvedShapeExpression::Primitive(kind),

            ShapeExpression::Applied { base, args } => todo!(),
        }
    }

    pub fn resolve_mapping(&mut self, mapping:Mapping, scope:ScopeId) -> ResolvedMapping {
        ResolvedMapping{
            from: self.get_symbol(scope, mapping.from_slot.clone()).expect(format!("Symbol not found: {:?}", mapping.from_slot).as_str()).clone(), 
            to: self.get_symbol(scope, mapping.to_slot.clone()).expect(format!("Symbol not found: {:?}",  mapping.to_slot).as_str()).clone()
        }
    }

    pub fn resolve_statement(&mut self, statement:Statement, scope:ScopeId) -> ResolvedStatement {
        match statement {
            Statement::Using { package } => {
                let source = fs::read_to_string(format!("/workspaces/SlotBox/src/{}.az", package));
                let tokens = tokenizer::tokenize(&source.unwrap_or_else(|_| panic!("Failed to read source file: {:?}.az", package)));
                let ast = parser::parse(tokens);
                ResolvedStatement::Using{ ast: self.analyze(ast) }
            }
            Statement::DeclareShape { name, slot_ids, mappings, generics } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { panic!("Symbol already present: {:?}", name); }
    
                let symbol = self.declare_shape(scope, name, slot_ids, mappings, generics);
                ResolvedStatement::DeclareShape{ symbol:  symbol.clone() }
            }
            Statement::DeclareObject { name } => {
                let my_scope = self.get_scope_mut(scope);
                if my_scope.symbols.contains_key(&name) { panic!("Symbol already present: {:?}", name); }
    
                let symbol = self.declare_object(scope, name);
                ResolvedStatement::DeclareObject{ symbol: symbol.clone() }
            }
            Statement::Attach { object, shape } => 
                ResolvedStatement::Attach{ 
                    object: self.resolve_expression(object, scope), 
                    shape: self.resolve_shape_expression(shape, scope),
                },
            Statement::Detach { object, shape } => 
                ResolvedStatement::Detach{ 
                    object: self.resolve_expression(object, scope), 
                    shape: self.resolve_shape_expression(shape, scope)
                },
            Statement::AddMapping { object, mapping } => 
                ResolvedStatement::AddMapping{ 
                    object: self.resolve_expression(object, scope), 
                    mapping: self.resolve_mapping(mapping, scope),
                },
            Statement::AttachWithRemap { object, shape, mappings } => {
                let mut resolved_mappings = Vec::new();
                for mapping in mappings {
                    resolved_mappings.push(self.resolve_mapping(mapping, scope));
                }
                ResolvedStatement::AttachWithRemap{ 
                    object: self.resolve_expression(object, scope), 
                    shape: self.resolve_shape_expression(shape, scope), 
                    mappings: resolved_mappings,
                }
            },
            Statement::Print { expr } => ResolvedStatement::Print{expr: self.resolve_expression(expr, scope)},
            Statement::If { condition, true_statement, else_statement } => {
                let resolved_condition = self.resolve_expression(condition, scope);
                let true_scope: u32 = self.create_scope(scope);
                let resolved_true = self.resolve_statement(*true_statement, true_scope);
                let else_scope = self.create_scope(scope);
                let resolved_else = self.resolve_statement(*else_statement, else_scope);
                ResolvedStatement::If{condition: resolved_condition, true_statement: Box::new(resolved_true), else_statement: Box::new(resolved_else)}
            }
            Statement::While { condition, statement } => {
                let resolved_condition = self.resolve_expression(condition, scope);
                let new_scope = self.create_scope(scope);
                let resolved_statement = self.resolve_statement(*statement, new_scope);
                ResolvedStatement::While{condition: resolved_condition, statement: Box::new(resolved_statement)}
            }
            
            Statement::For { local, statement } => todo!(),

            Statement::Assign {target, value } => 
                ResolvedStatement::Assign{ 
                    target: Box::new(self.resolve_expression(target, scope)), 
                    value: Box::new(self.resolve_expression(value, scope)),
                },
        }
    }

    fn analyze(&mut self, statements: Vec<Statement>) -> Vec<ResolvedStatement> {
        let mut resolved = Vec::new();

        for statement in statements {
            resolved.push(self.resolve_statement(statement, 0));
        }
    
        println!("\n Resolved Ast: \n{:?}", resolved);
        resolved
    }
}

pub fn analyze(statements: Vec<Statement>) -> Vec<ResolvedStatement> {
    let mut analyzer = Analyzer::new();
    analyzer.analyze(statements)
}

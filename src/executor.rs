use crate::tokenizer::Operator;
use crate::{
    Mapping, ObjectId, PrimitiveValue, Runtime, ShapeId, Value, ValueKind, executor,
    analyzer::{ResolvedExpression, ResolvedShapeExpression, ResolvedStatement, Symbol},
};
use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ShapeInstance {
    pub(crate) id: ShapeId,
    pub(crate) generics: Vec<ValueKind>,
}

pub const OBJECT_INSTANCE: ShapeInstance = ShapeInstance{id:0, generics:Vec::new()};

pub fn evaluate_shape(runtime: &Runtime, shape:ResolvedShapeExpression) -> ValueKind {
    shape.kind()
}

pub fn evaluate(runtime: &Runtime, expression:ResolvedExpression) -> Value {
    match expression {
        ResolvedExpression::Literal(value) => value,
        ResolvedExpression::Array(expressions) => {
            let mut values = Vec::new();
            for item in expressions {
                values.push(Box::new(evaluate(runtime, item)));
            }
            Value::Array(values)
        },
        ResolvedExpression::Variable(Symbol::Object(k)) => Value::Single(PrimitiveValue::ObjectId(OBJECT_INSTANCE, k.id)),

        ResolvedExpression::UnaryOp { operator, operand } => {
            match (operator, evaluate(runtime, *operand)) {
                (op, Value::Single(PrimitiveValue::Bool(val))) => 
                    match op {
                        Operator::Not => (!val).into(),
                        other => panic!("Invalid unary operator on bool: {:?}{:?}", other, val),
                    },
                (op, Value::Single(PrimitiveValue::Int32(val))) => 
                    match op {
                        Operator::Inc => (val + 1).into(),
                        Operator::Dec => (val - 1).into(),
                        Operator::BWNot => (!val).into(),
                        other => panic!("Invalid unary operator on int32: {:?}{:?}", other, val),
                    },
                _ => Value::Empty
            }
        },

        ResolvedExpression::BinaryOp { left, operator, right } => {
            match (evaluate(runtime, *left), operator, evaluate(runtime, *right)) { 
                
                // Bool - Bool
                (Value::Single(PrimitiveValue::Bool(left)), op, Value::Single(PrimitiveValue::Bool(right))) => 
                    match op {
                        Operator::Equal => (left == right).into(),
                        Operator::NEqual => (left != right).into(),
                        Operator::And => (left && right).into(),
                        Operator::Or => (left || right).into(),
                        other => panic!("Invalid binary operator on bools: {:?} {:?} {:?}", left, other, right),
                    },
                      
                // Int - Int
                (Value::Single(PrimitiveValue::Int32(left)), op, Value::Single(PrimitiveValue::Int32(right))) => 
                    match op {
                        Operator::Equal => (left == right).into(),
                        Operator::NEqual => (left != right).into(),
                        Operator::LT => (left < right).into(),
                        Operator::GT => (left > right).into(),
                        Operator::LTE => (left <= right).into(),
                        Operator::GTE => (left >= right).into(),
                        
                        Operator::Add => (left + right).into(),
                        Operator::Sub => (left - right).into(),
                        Operator::Mul => (left * right).into(),
                        Operator::Div => (left / right).into(),
                        Operator::Mod => (left % right).into(),

                        Operator::BWAnd => (left & right).into(),
                        Operator::BWOr => (left | right).into(),
                        Operator::BWXor => (left ^ right).into(),
                        Operator::BWShiftL => (left << right).into(),
                        Operator::BWShiftR => (left >> right).into(),
                        
                        Operator::Range => create_range(left, right),
                        Operator::RangeLT => create_range(left, right - 1),
                        
                        other => panic!("Invalid binary operator on ints: {:?} {:?} {:?}", left, other, right),
                    },
                    
                // String - String
                (Value::Single(PrimitiveValue::String(left)), op, Value::Single(PrimitiveValue::String(right))) => 
                    match op {
                        Operator::Equal => (left == right).into(),
                        Operator::NEqual => (left != right).into(),
                        
                        Operator::Add => (left + &right).into(),
                        
                        other => panic!("Invalid operator {:?}", other)
                    },

                _ => Value::Empty
            }
        },
        
        ResolvedExpression::MemberAccess{ target, member} => {
            match (evaluate(runtime, *target), member) {
                (Value::Single(PrimitiveValue::ObjectId(_, id)), Symbol::Azimuth(azimuth)) => {
                    runtime.get_slot_value(id, azimuth.id).unwrap_or(&Value::Empty).clone()
                }
                (other, member) => panic!("Member access not permitted for {:?}.{:?}", other, member)
            }
        }

        other => panic!("Invalid expression: {:?}", other)
    }
}

pub fn evaluate_mut(runtime: &mut Runtime, expression:ResolvedExpression) -> Option<&mut Value> {
    match expression {
        ResolvedExpression::MemberAccess{ target, member} => {
            match (evaluate(runtime, *target), member) {
                (Value::Single(PrimitiveValue::ObjectId(_, id)), Symbol::Azimuth(azimuth)) => {
                    runtime.get_slot_value_mut(id, azimuth.id)
                }
                (other, member) => panic!("Member access not permitted for {:?}.{:?}", other, member)
            }
        }
        other => panic!("Invalid mutable expression: {:?}", other)
    }
}

pub fn create_range(from: i32, to: i32) -> Value {
    let mut range = Vec::new();
    for i in from..=to {
        range.push(Box::new(i.into()));
    }
    Value::Array(range)
}

pub fn execute(runtime: &mut Runtime, ast: Vec<ResolvedStatement>) {
    for statement in ast {
        execute_statement(runtime, statement);
    }
}

pub fn execute_statement(runtime: &mut Runtime, statement: ResolvedStatement) {
    match statement {
        ResolvedStatement::Using { ast} => {
            executor::execute(runtime, ast);
        },

        ResolvedStatement::Print { expr } => {
            match evaluate(runtime, expr) {
                Value::Single(PrimitiveValue::String(k)) => println!("{:?}", k),
                Value::Single(PrimitiveValue::ObjectId(_, id)) => runtime.print_object(id),
                other => panic!("Failed to print {:?}", other),
            }
        },

        ResolvedStatement::DeclareShape { symbol: Symbol::Shape(info) } => {
            runtime.create_shape(info);
        },

        ResolvedStatement::DeclareObject { symbol: Symbol::Object(id) } => {
            runtime.create_object(id);
        },

        ResolvedStatement::Attach { object, shape} => {
            match (evaluate(runtime, object), evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::ObjectId(_, object_id)), ValueKind::Shape(shape_inst)) => runtime.attach_shape(object_id, shape_inst),
                (object, shape) => panic!("Could not attach {:?} to {:?}", shape, object)
            }
        },

        ResolvedStatement::Detach { object, shape } => {
            match (evaluate(runtime, object), evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::ObjectId(_, object_id)), ValueKind::Shape(shape_inst)) => runtime.detach_shape(object_id, shape_inst),
                (object, shape) => panic!("Could not detach {:?} from {:?}", shape, object)
            }
        },

        ResolvedStatement::AddMapping { object, mapping } => {
            match (evaluate(runtime, object), mapping.from, mapping.to) {
                (Value::Single(PrimitiveValue::ObjectId(_, id)), Symbol::Azimuth(from), Symbol::Azimuth(to))
                    => runtime.remap_slot(id, to.id, from.id),
                (object, from, to) => panic!("Invalid mapping: {:?}, {:?} -> {:?}", object, from, to),
            }
        },

        ResolvedStatement::AttachWithRemap { object, shape, mappings } => {
            match (evaluate(runtime, object), evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::ObjectId(_, object_id)), ValueKind::Shape(shape_inst)) => {
                    
                    let mut remap = Vec::new();
                    for mapping in mappings {
                        match (mapping.from, mapping.to) {
                            (Symbol::Azimuth(from), Symbol::Azimuth(to)) => {
                                remap.push(Mapping{from: from.id, to: to.id});
                            }
                            _ => todo!()
                        }
                    }

                    runtime.attach_shape_with_remap(object_id, shape_inst, remap);

                },
                (object, shape) => panic!("Could not attach {:?} to {:?}", shape, object)
            }
        },
        
        ResolvedStatement::Assign { target, value } => {
            let new_val = evaluate(runtime, *value);

            match evaluate_mut(runtime, *target) {
                Some(target_val) => {
                    *target_val = new_val;
                }
                other => panic!("Could not assign {:?} to {:?}", new_val, other),
            }
        }

        other => panic!("Invalid statement: {:?}", other)
    }
}
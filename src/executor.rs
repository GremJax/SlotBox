use crate::Function;
use crate::tokenizer::{Operator, Span};
use crate::{
    Mapping, ObjectId, PrimitiveValue, Runtime, ShapeId, Value, ValueKind, executor,
    analyzer::{ResolvedExpression, ResolvedShapeExpression, ResolvedStatement, Symbol},
};
use std::collections::{HashMap, HashSet};
use std::{fs, usize};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Error { span: Span, message: String },
    Throw { span: Span, message: String },
    TypeMismatch { span: Span, found: Value, expected: ValueKind },
    UnexpectedBreakout { span: Span },
    InvalidOperator { span: Span, operator: Operator, operand: ValueKind },
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Error { span, message } =>
                write!(f, "{}: {}", span, message),
            RuntimeError::Throw { span, message } =>
                write!(f, "{}: Throw: \"{}\"", span, message),
            RuntimeError::TypeMismatch { span, found, expected } =>
                write!(f, "{}: {:?} does not match expected type {:?}", span, found, expected),
            RuntimeError::UnexpectedBreakout { span } =>
                write!(f, "{}: Unexpected breakout", span),
            RuntimeError::InvalidOperator { span, operator, operand } =>
                write!(f, "{}: {:?} is invalid operator for {:?}", span, operator, operand),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ShapeInstance {
    pub id: ShapeId,
    pub generics: Vec<ValueKind>,
}

pub const OBJECT_INSTANCE: ShapeInstance = ShapeInstance{id:0, generics:Vec::new()};

pub fn evaluate_shape(runtime: &Runtime, shape:ResolvedShapeExpression) -> ValueKind {
    shape.kind()
}

pub fn evaluate(runtime: &mut Runtime, expression:ResolvedExpression) -> Result<Value, RuntimeError> {
    match expression {
        ResolvedExpression::Value(_, value) => Ok(value),
        ResolvedExpression::Array(_, expressions, kind) => {
            let mut values = Vec::new();
            for item in expressions {
                values.push(Box::new(evaluate(runtime, item)?));
            }
            Ok(Value::Array(values, kind))
        },
        ResolvedExpression::Variable(_, Symbol::Object(k)) => Ok(Value::Single(PrimitiveValue::Object(k.id, ValueKind::Shape(OBJECT_INSTANCE)))),
        ResolvedExpression::Variable(_, Symbol::Local(k)) => Ok(runtime.get_local(k.id).clone()),

        ResolvedExpression::UnaryOp { span, operator, operand } => {
            match (operator, evaluate(runtime, *operand)?) {
                (op, Value::Single(PrimitiveValue::Bool(val))) => 
                    match op {
                        Operator::Not => Ok((!val).into()),
                        operator => Err(RuntimeError::InvalidOperator { span, operator, operand:ValueKind::Bool })
                    },
                (op, Value::Single(PrimitiveValue::Int32(val))) => 
                    match op {
                        Operator::Inc => Ok((val + 1).into()),
                        Operator::Dec => Ok((val - 1).into()),
                        Operator::BWNot => Ok((!val).into()),
                        operator => Err(RuntimeError::InvalidOperator { span, operator, operand:ValueKind::Int32 })
                    },

                (operator, operand) => Err(RuntimeError::Error{ span, message: format!("Invalid operation: {:?} {:?}", operator, operand) })
            }
        },

        ResolvedExpression::BinaryOp { span, left, operator, right } => {
            match (evaluate(runtime, *left)?, operator, evaluate(runtime, *right)?) { 
                
                // Bool - Bool
                (Value::Single(PrimitiveValue::Bool(left)), op, Value::Single(PrimitiveValue::Bool(right))) => 
                    match op {
                        Operator::Equal => Ok((left == right).into()),
                        Operator::NEqual => Ok((left != right).into()),
                        Operator::And => Ok((left && right).into()),
                        Operator::Or => Ok((left || right).into()),
                        operator => Err(RuntimeError::InvalidOperator { span, operator, operand: ValueKind::Bool }),
                    },
                      
                // Int - Int
                (Value::Single(PrimitiveValue::Int32(left)), op, Value::Single(PrimitiveValue::Int32(right))) => 
                    match op {
                        Operator::Equal => Ok((left == right).into()),
                        Operator::NEqual => Ok((left != right).into()),
                        Operator::LT => Ok((left < right).into()),
                        Operator::GT => Ok((left > right).into()),
                        Operator::LTE => Ok((left <= right).into()),
                        Operator::GTE => Ok((left >= right).into()),
                        
                        Operator::Add => Ok((left + right).into()),
                        Operator::Sub => Ok((left - right).into()),
                        Operator::Mul => Ok((left * right).into()),
                        Operator::Div => Ok((left / right).into()),
                        Operator::Mod => Ok((left % right).into()),

                        Operator::BWAnd => Ok((left & right).into()),
                        Operator::BWOr => Ok((left | right).into()),
                        Operator::BWXor => Ok((left ^ right).into()),
                        Operator::BWShiftL => Ok((left << right).into()),
                        Operator::BWShiftR => Ok((left >> right).into()),
                        
                        Operator::Range => Ok(create_range(left, right)),
                        Operator::RangeLT => Ok(create_range(left, right - 1)),
                        
                        operator => Err(RuntimeError::InvalidOperator { span, operator, operand: ValueKind::Int32 }),
                    },
                    
                // String - String
                (Value::Single(PrimitiveValue::String(left)), op, Value::Single(PrimitiveValue::String(right))) => 
                    match op {
                        Operator::Equal => Ok((left == right).into()),
                        Operator::NEqual => Ok((left != right).into()),
                        
                        Operator::Add => Ok((left + &right).into()),
                        
                        operator => Err(RuntimeError::InvalidOperator { span, operator, operand: ValueKind::String }),
                    },

                (left, op, right) => Err(RuntimeError::Error{ span, message: format!("Invalid operation: {:?} {:?} {:?}", left, op, right) })
            }
        },
        
        ResolvedExpression::MemberAccess{ span, target, member} => {
            match (evaluate(runtime, *target)?, member) {
                (Value::Single(PrimitiveValue::Object(object_id, _)), Symbol::Azimuth(azimuth)) => {
                    match runtime.get_slot_value(object_id, azimuth.id) {
                        Some(value) => Ok(value.clone()),
                        None => return Err(RuntimeError::Error{span, message:format!("Member {:?} not found for {:?}", azimuth.name, object_id)}),
                    }
                }
                (other, member) => Err(RuntimeError::Error{span, message:format!("Member access not permitted for {:?}.{:?}", other, member)}),
            }
        },
        
        ResolvedExpression::ArrayAccess{ span, target, index} => {
            let target = evaluate(runtime, *target)?;
            let index = evaluate(runtime, *index)?;

            match (target, index) {
                (Value::Array(array, _), Value::Single(PrimitiveValue::Int32(index))) => {
                    match array.get(index as usize) {
                        Some(value) => Ok(*value.clone()),
                        None => return Err(RuntimeError::Error{span, message:format!("Index {:?} out of bounds", index)}),
                    }
                }
                (other, member) => Err(RuntimeError::Error{span, message:format!("Array access not permitted for {:?}.{:?}", other, member)}),
            }
        },
        
        ResolvedExpression::FunctionCall{ span, target, args} => {
            let mut params = Vec::new();
            for arg in args {
                params.push(evaluate(runtime, arg)?);
            }

            let target = evaluate(runtime, *target)?;

            let func = match target {
                Value::Single(PrimitiveValue::Function(func)) => func,
                other => return Err(RuntimeError::Error{span, message:format!("{:?} is not a function", other)}),
            };

            let statement = &func.func;
            let expected_return = func.output_type.clone();

            // Create locals
            for param in &params {
                runtime.reserve_local(param.clone());
            }

            match execute_statement(runtime, statement.clone())? {
                ExecFlow::Error { span, message } => Err(RuntimeError::Throw { span, message }),
                ExecFlow::Return(span, Value::Single(value)) => {
                    if value.kind() != expected_return {
                        return Err(RuntimeError::TypeMismatch { span, found: Value::Single(value), expected: expected_return });
                    }

                    // Free locals
                    runtime.clear_locals();

                    Ok(Value::Single(value))
                },
                _ => Err(RuntimeError::Error { span, message:format!("No value returned, expected {:?}", expected_return) }),
            }
        },

        ResolvedExpression::Function{ span, has_self, input_types, output_type, func } => {
            let output_type = evaluate_shape(runtime, output_type);
            let mut inputs = Vec::new();
            for input in input_types {
                inputs.push(evaluate_shape(runtime, input));
            }
            
            let function = Function{ 
                id: 0, 
                has_self, 
                input_types:inputs, 
                output_type, 
                func: *func,
            };
            Ok(Value::Single(PrimitiveValue::Function(Box::new(function))))
        }

        other => panic!("Invalid expression: {:?}", other)
    }
}

pub fn evaluate_place(runtime: &mut Runtime, expression:ResolvedExpression) -> Result<Value, RuntimeError> {
    match expression {
        ResolvedExpression::MemberAccess{ span, target, member} => {
            match (evaluate(runtime, *target)?, member) {
                (Value::Single(PrimitiveValue::Object(object_id, kind)), Symbol::Azimuth(azimuth)) => {
                    Ok(Value::Single(PrimitiveValue::Pointer(object_id, azimuth.id, kind)))
                }
                (other, member) => Err(RuntimeError::Error{span, message:format!("Member access not permitted for {:?}.{:?}", other, member)}),
            }
        },
        ResolvedExpression::ArrayAccess{ span, target, index} => {
            let target = evaluate_place(runtime, *target)?;
            let index = evaluate(runtime, *index)?;
            
            let i = if let Value::Single(PrimitiveValue::Int32(index)) = index {
                index as usize
            } else { return Err(RuntimeError::TypeMismatch { span, found: index, expected: ValueKind::Int32 }) };
            
            match target {
                Value::Single(PrimitiveValue::Pointer(object_id, azimuth_id, kind)) => {
                    Ok(Value::Single(PrimitiveValue::ArrayElement(object_id, azimuth_id, kind, i)))
                }
                other => Err(RuntimeError::Error{span, message:format!("Array access not permitted for {:?}[{}]", other, i)})
            }
            
        },
        other => evaluate(runtime, other), 
    }
}

pub fn create_range(from: i32, to: i32) -> Value {
    let mut range = Vec::new();
    for i in from..=to {
        range.push(Box::new(i.into()));
    }
    Value::Array(range, ValueKind::Int32)
}

pub fn execute(runtime: &mut Runtime, ast: Vec<ResolvedStatement>) -> Result<ExecFlow, RuntimeError> {
    for statement in ast {
        match execute_statement(runtime, statement)? {
            ExecFlow::Normal(_) => {},
            ExecFlow::Break(span) => {
                return Err(RuntimeError::UnexpectedBreakout{span});
            },
            ExecFlow::Continue(span) => {
                return Err(RuntimeError::UnexpectedBreakout{span});
            },
            ExecFlow::Return(span, _) => {
                return Err(RuntimeError::UnexpectedBreakout{span});
            },
            ExecFlow::Error { span, message } => {
                return Err(RuntimeError::Throw{span, message});
            }
        }
    }
    Ok(ExecFlow::Normal(Span{line:0,column:0}))
}

#[derive(Debug, Clone)]
pub enum ExecFlow {
    Normal(Span),
    Break(Span),
    Continue(Span),
    Return(Span, Value),
    Error{span:Span, message:String},
}

pub fn execute_statement(runtime: &mut Runtime, statement: ResolvedStatement) -> Result<ExecFlow, RuntimeError> {
    match statement {
        ResolvedStatement::Using { span, ast} => {
            executor::execute(runtime, ast)?;
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::Print { span, expr } => {
            match evaluate(runtime, expr)? {
                Value::Single(PrimitiveValue::String(k)) => println!("{}", k),
                Value::Single(PrimitiveValue::Object(object_id, _)) => runtime.print_object(object_id),
                Value::Single(k) => println!("{:?}", k),
                Value::Array(k, _) => println!("{:?}", k),
                other => { return Err(RuntimeError::TypeMismatch{span, found: other, expected: ValueKind::String}); }
            }
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::DeclareShape { span, symbol: Symbol::Shape(info), azimuths } => {
            runtime.create_shape(info);

            for azimuth in azimuths {
                if let Symbol::Azimuth(info) = azimuth {
                    runtime.create_azimuth(info);
                }
            }

            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::DeclareObject { span, symbol: Symbol::Object(info), shape } => {
            let id = info.id.clone();

            runtime.create_object(info);

            if let ValueKind::Shape(inst) = evaluate_shape(runtime, shape){
                runtime.attach_shape(id, inst);
            }
            
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::Attach { span, object, shape} => {
            match (evaluate(runtime, object)?, evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::Object(object_id, _)), ValueKind::Shape(shape_inst)) => 
                    runtime.attach_shape(object_id, shape_inst),
                (object, shape) => return Err(RuntimeError::Error{span, message:format!("Could not attach {:?} to {:?}", shape, object)})
            }
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::Detach { span, object, shape } => {
            match (evaluate(runtime, object)?, evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::Object(object_id, _)), ValueKind::Shape(shape_inst)) => runtime.detach_shape(object_id, shape_inst),
                (object, shape) => return Err(RuntimeError::Error{span, message:format!("Could not detach {:?} from {:?}", shape, object)})
            }
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::AddMapping { span, object, mapping } => {
            match (evaluate(runtime, object)?, mapping.from, mapping.to) {
                (Value::Single(PrimitiveValue::Object(object_id, _)), Symbol::Azimuth(from), Symbol::Azimuth(to))
                    => runtime.remap_slot(object_id, to.id, from.id),
                (object, from, to) => return Err(RuntimeError::Error{span, message:format!("Invalid mapping: {:?}, {:?} -> {:?}", object, from, to)}),
            }
            Ok(ExecFlow::Normal(span))
        },

        ResolvedStatement::AttachWithRemap { span, object, shape, mappings } => {
            match (evaluate(runtime, object)?, evaluate_shape(runtime, shape)) {
                (Value::Single(PrimitiveValue::Object(object_id, _)), ValueKind::Shape(shape_inst)) => {
                    
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
                (object, shape) => return Err(RuntimeError::Error{span, message:format!("Could not attach {:?} to {:?}", shape, object)})
            }
            Ok(ExecFlow::Normal(span))
        },
        
        ResolvedStatement::Assign { span, target, value } => {
            let val = evaluate(runtime, value)?;

            match evaluate_place(runtime, target)? {
                Value::Single(PrimitiveValue::Pointer(object_id, az, kind)) => {
                    runtime.set_slot_value(object_id, az, val);
                }
                Value::Single(PrimitiveValue::ArrayElement(obj, az, kind, i)) => {
                    runtime.set_slot_value_array_element(obj, az, i, val);
                }
                other => return Err(RuntimeError::Error{span, message:format!("Could not assign {:?} to {:?}", val, other)}),
            }
            Ok(ExecFlow::Normal(span))
        }

        ResolvedStatement::If { span, condition, true_statement, else_statement } => {
            let cond = evaluate(runtime, condition)?;

            match cond {
                Value::Single(PrimitiveValue::Bool(true)) => execute_statement(runtime, *true_statement),
                Value::Single(PrimitiveValue::Bool(false)) => {
                    if let Some(statement) = *else_statement {
                        return execute_statement(runtime, statement)
                    }
                    Ok(ExecFlow::Normal(span))
                }
                other => return Err(RuntimeError::Error{span, message:format!("If condition was not true or false: {:?}", other)}),
            }
        }

        ResolvedStatement::While { span, condition, statement } => {
            let mut flag = true;
            while flag {
                flag = match evaluate(runtime, condition.clone())? {
                    Value::Single(PrimitiveValue::Bool(true)) => true,
                    Value::Single(PrimitiveValue::Bool(false)) => false,
                    other => return Err(RuntimeError::Error{span, message:format!("While condition was not true or false: {:?}", other)}),
                };

                if flag { 
                    match execute_statement(runtime, *statement.clone())? {
                        ExecFlow::Normal(_) => {},
                        ExecFlow::Break(_) => break,
                        ExecFlow::Continue(_) => continue,
                        ExecFlow::Return(span, value) => return Ok(ExecFlow::Return(span, value)),
                        ExecFlow::Error{span, message} => return Ok(ExecFlow::Error{span, message})
                    }
                }
            }
            Ok(ExecFlow::Normal(span))
        }

        ResolvedStatement::For{ } => todo!(),

        ResolvedStatement::Block(statements) => {
            let mut last_span = Span{line: 0, column: 0};
            for statement in statements{
                match execute_statement(runtime, statement)? {
                    ExecFlow::Normal(span) => last_span = span,
                    flow => return Ok(flow),
                }
            }
            Ok(ExecFlow::Normal(last_span))
        }

        ResolvedStatement::Break { span } => Ok(ExecFlow::Break(span)),
        ResolvedStatement::Continue { span } => Ok(ExecFlow::Continue(span)),
        ResolvedStatement::Return { span, value } => Ok(ExecFlow::Return(span, evaluate(runtime, value)?)),
        ResolvedStatement::Throw { span, message } => {
            match evaluate(runtime, message)? {
                Value::Single(PrimitiveValue::String(message)) => Ok(ExecFlow::Error{span, message}),
                other => Err(RuntimeError::TypeMismatch{span, found: other, expected: ValueKind::String})
            }
        }

        // Adorable ignores
        //ResolvedStatement::Break => {}
        //ResolvedStatement::Continue => {}
        //ResolvedStatement::Return(_) => {}

        other => return Err(RuntimeError::Error{span: Span{line:0,column:0}, message:format!("Invalid statement: {:?}", other)})
    }
}
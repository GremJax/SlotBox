use crate::ObjectId;
use crate::PrimitiveValue;
use crate::ShapeId;
use crate::Azimuth;
use crate::executor;
use crate::parser;
use crate::parser::{Statement, Expression};
use crate::Value;
use crate::Runtime;
use crate::tokenizer;
use std::collections::{HashMap, HashSet};
use std::fs;

pub enum Identifier {
    Object(ObjectId),
    Shape(ShapeId),
    Slot(Azimuth),
}

pub fn evaluate(expression:Expression, runtime: &mut Runtime, identifier_map: &HashMap<String, Identifier>) -> Value {
    match expression {
        Expression::Literal(value) => value,
        Expression::Array(expressions) => {
            let mut values = Vec::new();
            for item in expressions {
                values.push(Box::new(evaluate(item, runtime, identifier_map)));
            }
            Value::Array(values)
        },
        Expression::Variable(_) => todo!(),
        Expression::UnaryOp { operator, operand } => {
            match (operator, evaluate(*operand, runtime, identifier_map)) {
                (op, Value::Single(PrimitiveValue::Bool(val))) if op == "!" => Value::Single(PrimitiveValue::Bool(!val)),
                _ => Value::Empty
            }
        }
        Expression::BinaryOp { left, operator, right } => {
            match (evaluate(*left, runtime, identifier_map), operator, evaluate(*right, runtime, identifier_map)) {
                (Value::Single(PrimitiveValue::Bool(left)), op, Value::Single(PrimitiveValue::Bool(right))) => 
                    match op.as_str() {
                        "==" => Value::Single(PrimitiveValue::Bool(left == right)),
                        "!=" => Value::Single(PrimitiveValue::Bool(left != right)),
                        "&&" => Value::Single(PrimitiveValue::Bool(left && right)),
                        "||" => Value::Single(PrimitiveValue::Bool(left || right)),
                        other => panic!("Invalid operator {}", other)
                    }
                (Value::Single(PrimitiveValue::Int32(left)), op, Value::Single(PrimitiveValue::Int32(right))) => 
                    match op.as_str() {
                        "==" => Value::Single(PrimitiveValue::Bool(left == right)),
                        "!=" => Value::Single(PrimitiveValue::Bool(left != right)),
                        "<" => Value::Single(PrimitiveValue::Bool(left < right)),
                        ">" => Value::Single(PrimitiveValue::Bool(left > right)),
                        "<=" => Value::Single(PrimitiveValue::Bool(left <= right)),
                        ">=" => Value::Single(PrimitiveValue::Bool(left >= right)),
                        
                        "+" => Value::Single(PrimitiveValue::Int32(left + right)),
                        "-" => Value::Single(PrimitiveValue::Int32(left - right)),
                        "*" => Value::Single(PrimitiveValue::Int32(left * right)),
                        "/" => Value::Single(PrimitiveValue::Int32(left / right)),
                        "%" => Value::Single(PrimitiveValue::Int32(left % right)),
                        other => panic!("Invalid operator {}", other)
                    }
                _ => Value::Empty
            }
        }
        Expression::SlotAccess { object, slot } => todo!(),
        Expression::CallStack { stack } => evaluate_call_stack(stack, runtime, identifier_map),
    }
}

pub fn evaluate_call_stack(stack: Vec<String>, runtime: &mut Runtime, identifier_map: &HashMap<String, Identifier>) -> Value {
let mut current_val = Value::Empty;
    let mut shape_clarifier;

    let mut taken_path = false;

    for identifier in stack {
        match identifier_map.get(&identifier)  {
            None => panic!("Unknown identifier: {}", identifier),
            Some(Identifier::Object(object)) => {
                if taken_path { panic!("Invalid object access: {}", identifier) }
                taken_path = true;
                current_val = Value::Single(PrimitiveValue::ObjectId(0, *object));
            },
            Some(Identifier::Shape(shape_id)) => {
                if(!taken_path) {
                    let shape = runtime.get_shape(*shape_id);
                    current_val = Value::Single(PrimitiveValue::ObjectId(*shape_id, shape.static_object_id));
                }
                shape_clarifier = shape_id;
            },
            Some(Identifier::Slot(slot)) => {
                if slot.is_static && taken_path {
                    panic!("Static value accessed mid call: {}", identifier);
                }

                match current_val{
                    Value::Single(PrimitiveValue::ObjectId(_, id)) => {
                        current_val = runtime.get_slot_value(id, slot.clone())
                            .unwrap_or_else(|| panic!("Object does not have mapping for {}", identifier)).clone();
                    },
                    other => panic!("Can't get {} from {:?}", slot.name, other)
                }
            },
        }
    }
    current_val
}

pub fn execute(statements: Vec<Statement>, runtime: &mut Runtime, identifier_map: &mut HashMap<String, Identifier>) {
    for stmt in statements {
        match stmt {
            Statement::Using { package } => {
                let source = fs::read_to_string(format!("/workspaces/SlotBox/src/{}.az", package));
                let tokens = tokenizer::tokenize(&source.unwrap_or_else(|_| panic!("Failed to read source file: {:?}.az", package)));
                let ast = parser::parse(tokens);
                executor::execute(ast, runtime, identifier_map);
            },

            Statement::Print { object } => {
                if let Some(Identifier::Object(object_id)) = identifier_map.get(&object) {
                    runtime.print_object(*object_id);
                } else {
                    println!("Error: Object '{}' not found", object);
                }
            },
            Statement::PrintString { string } => {
                println!("{}", string);
            },

            Statement::DeclareShape { name, slot_ids, mappings } => {
                let shape_id = runtime.create_shape(name.clone());
                identifier_map.insert(name, Identifier::Shape(shape_id));

                for slot_id in slot_ids {
                    let identifier = runtime.define_slot_on_shape(shape_id, slot_id.name.clone(), slot_id.value_type, slot_id.is_static);
                    identifier_map.insert(slot_id.name.clone(), Identifier::Slot(identifier.clone()));
                    
                    if let Some(expr) = slot_id.set_value {
                        let value = evaluate(expr, runtime, identifier_map);
                        let shape = runtime.get_shape(shape_id);
                        println!("{} is shapes number ", shape.static_object_id);
                        runtime.set_slot_value(shape.static_object_id, identifier, value);
                    }
                }
            },
            Statement::DeclareShapeWithGenerics { name, slot_ids, mappings, generics} => {
                let shape_id = runtime.create_shape_with_generics(name.clone(), generics.len() as u8);
                identifier_map.insert(name, Identifier::Shape(shape_id));

                for slot_id in slot_ids {
                    let identifier = runtime.define_slot_on_shape(shape_id, slot_id.name.clone(), slot_id.value_type, slot_id.is_static);
                    identifier_map.insert(slot_id.name.clone(), Identifier::Slot(identifier.clone()));
                    
                    if let Some(expr) = slot_id.set_value {
                        let value = evaluate(expr, runtime, identifier_map);
                        let shape = runtime.get_shape(shape_id);
                        println!("{} is shapes number ", shape.static_object_id);
                        runtime.set_slot_value(shape.static_object_id, identifier, value);
                    }
                }
            }
            Statement::DeclareObject { name} => {
                let object_id = runtime.create_object(name.clone());
                identifier_map.insert(name, Identifier::Object(object_id));
            },

            Statement::Attach { object, shape, generics } => {
                if let (Some(Identifier::Object(object_id)), Some(Identifier::Shape(shape_id))) = (identifier_map.get(&object), identifier_map.get(&shape)) {
                    runtime.attach_shape(*object_id, *shape_id, generics);
                } else {
                    println!("Error: Object '{}' or Shape '{}' not found", object, shape);
                }
            },
            Statement::Detach { object, shape } => {
                if let (Some(Identifier::Object(object_id)), Some(Identifier::Shape(shape_id))) = (identifier_map.get(&object), identifier_map.get(&shape)) {
                    runtime.detach_shape(*object_id, *shape_id);
                } else {
                    println!("Error: Object '{}' or Shape '{}' not found", object, shape);
                }
            },
            Statement::AddMapping { object, to_slot, from_slot } => {
                if let (Some(Identifier::Object(object_id)), Some(Identifier::Slot(to_slot_id)), Some(Identifier::Slot(from_slot_id))) 
                    = (identifier_map.get(&object), identifier_map.get(&to_slot), identifier_map.get(&from_slot)) {
                    runtime.remap_slot(*object_id, &to_slot_id, &from_slot_id);
                } else {
                    println!("Error: Object '{}' or Slot '{}' not found", object, to_slot);
                }
            },
            Statement::AttachWithRemap { object, shape, mappings, generics } => {
                if let (Some(Identifier::Object(object_id)), Some(Identifier::Shape(shape_id))) = (identifier_map.get(&object), identifier_map.get(&shape)) {
                    // Find the remapped slots for the shape and create a new mapping list
                    let remap_mappings: Vec<crate::Mapping> = mappings.into_iter().map(|mapping| {
                        if let (Some(Identifier::Slot(from_slot_id)), Some(Identifier::Slot(to_slot_id))) = (identifier_map.get(&mapping.from_slot), identifier_map.get(&mapping.to_slot)) {
                            crate::Mapping { from_slot: from_slot_id.clone(), to_slot: to_slot_id.clone() }
                        } else {
                            panic!("Error: Slot '{}' not found for mapping", mapping.from_slot);
                        }
                    }).collect();
                    runtime.attach_shape_with_remap(*object_id, *shape_id, &remap_mappings, generics);
                } else {
                    println!("Error: Object '{}' or Shape '{}' not found", object, shape);
                }
            }
            Statement::Assign { object, slot, value } => {
                if let Some(Identifier::Object(object_id)) = identifier_map.get(&object) {
                    if let Some(Identifier::Slot(slot_id)) = identifier_map.get(&slot) {

                        let object = runtime.get_object(*object_id);
                        let val = evaluate(value, runtime, identifier_map);
                        runtime.set_slot_value(*object_id, slot_id.clone(), val);

                    } else {
                        println!("Error: Slot '{}' not found", slot);
                    }
                } else {
                    println!("Error: Object '{}' not found", object);
                }
            },
        }
    }
}
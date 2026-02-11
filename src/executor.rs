use crate::ObjectId;
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
                    identifier_map.insert(slot_id.name.clone(), Identifier::Slot(identifier));
                }
            },
            Statement::DeclareShapeWithGenerics { name, slot_ids, mappings, generics} => {
                let shape_id = runtime.create_shape_with_generics(name.clone(), generics.len() as u8);
                identifier_map.insert(name, Identifier::Shape(shape_id));

                for slot_id in slot_ids {
                    let identifier = runtime.define_slot_on_shape(shape_id, slot_id.name.clone(), slot_id.value_type, slot_id.is_static);
                    identifier_map.insert(slot_id.name.clone(), Identifier::Slot(identifier));
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
                    let object = runtime.get_object(*object_id);

                    if let Some(Identifier::Slot(slot_id)) = identifier_map.get(&slot) {
                        if let Expression::Literal(val) = value {
                            runtime.set_slot_value(*object_id, slot_id.clone(), val);
                        } else {
                            println!("Error: Failed to evaluate expression for slot '{}'", slot);
                        }
                    } else {
                        println!("Error: Slot '{}' not found", slot);
                    }
                } else {
                    println!("Error: Object '{}' not found", object);
                }
            },
        }
    }

    println!("Program successfully executed!");
}
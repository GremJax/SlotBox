use crate::ObjectId;
use crate::ShapeId;
use crate::SlotId;
use crate::parser::{Statement, Expression};
use crate::Value;
use crate::Runtime;
use std::collections::{HashMap, HashSet};

enum Identifier {
    Object(ObjectId),
    Shape(ShapeId),
    Slot(SlotId),
}

pub fn execute(statements: Vec<Statement>) {
    let mut runtime = Runtime::new();
    let mut identifier_map = HashMap::new();

    for stmt in statements {
        match stmt {
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

                let mut counter = 1;
                for slot_id in slot_ids {
                    let identifier = runtime.define_slot_on_shape(shape_id, counter, slot_id.name.clone(), slot_id.value_type, slot_id.is_static);
                    identifier_map.insert(slot_id.name.clone(), Identifier::Slot(identifier));
                    counter += 1;
                }
            },
            Statement::DeclareObject { name} => {
                let object_id = runtime.create_object(name.clone());
                identifier_map.insert(name, Identifier::Object(object_id));
            },
            Statement::Attach { object, shape } => {
                if let (Some(Identifier::Object(object_id)), Some(Identifier::Shape(shape_id))) = (identifier_map.get(&object), identifier_map.get(&shape)) {
                    runtime.attach_shape(*object_id, *shape_id);
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
            Statement::Assign { object, slot, value } => {
                if let Some(Identifier::Object(object_id)) = identifier_map.get(&object) {
                    if let Some(Identifier::Slot(slot_id)) = identifier_map.get(&slot) {
                        if let Expression::Literal(val) = value {
                            runtime.set_slot_value(*object_id, &slot_id, val);
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
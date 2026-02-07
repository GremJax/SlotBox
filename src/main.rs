use std::collections::{HashMap, HashSet};

// Runtime Value
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum ValueKind {
    Int32,Float32,Bool,String,ObjectId(ShapeId),SlotId,Function,None
}

#[derive(Default, Debug, Clone)]
enum Value {
    Int32(i32),
    Float32(f32),
    Bool(bool),
    String(String),
    ObjectId(ObjectId),
    SlotId(SlotId),
    Function(Function),
    #[default] None,
}

impl Value {
    fn kind(&self) -> ValueKind {
        match self {
            Value::Int32(_) => ValueKind::Int32,
            Value::Float32(_) => ValueKind::Float32,
            Value::Bool(_) => ValueKind::Bool,
            Value::String(_) => ValueKind::String,
            Value::ObjectId(_) => ValueKind::ObjectId(0), // Placeholder, actual ShapeId should be used
            Value::SlotId(_) => ValueKind::SlotId,
            Value::Function(_) => ValueKind::Function,
            Value::None => ValueKind::None,
        }
    }
}

// Native Function
struct FunctionParams {
    caller: ObjectId,
    inputs: Vec<Value>,
}

type NativeFn = fn(FunctionParams) -> Value;

#[derive(Debug, Clone)]
struct Function {
    self_type: ShapeId,
    input_types: Vec<ValueKind>,
    output_type: ValueKind,
    func: NativeFn,
}

// Slot Id
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct SlotId {
    shape_id: ShapeId,
    slot_num: u32,
    name: String,
    value_type: ValueKind,
    is_static: bool,
}

// Runtime Slot state
type SlotStateId = u32;

#[derive(Default)]
struct SlotState {
    present: bool,
    sealed: bool,
    value: Value,
}

// Shape
type ShapeId = u32;

struct Shape {
    id: ShapeId,
    name: String,
    
    defined_slots: HashSet<SlotId>,
    slot_remapping: HashMap<SlotId, SlotId>,
    static_object_id: ObjectId,
}

impl Shape {
    fn define_slot(&mut self, num: u32, name: String, 
        kind: ValueKind, is_static: bool) -> SlotId {
        let slot = SlotId {
            shape_id: self.id,
            slot_num: num,
            name: name,
            value_type: kind,
            is_static,
        };
        self.defined_slots.insert(slot.clone());
        slot
    }

    fn remap_slot(&mut self, from: &SlotId, to: &SlotId) {
        self.slot_remapping.insert(from.clone(), to.clone());
    }
}

// Object
type ObjectId = u32;

struct Object {
    id: ObjectId,
    name: String,

    slot_mapping: HashMap<SlotId, SlotStateId>,
    slot_states: Vec<SlotState>,
    free_slots: Vec<SlotStateId>,
    next_slot_state_id: SlotStateId,
}

impl Object {
    fn allocate_slot(&mut self) -> SlotStateId {
        if let Some(id) = self.free_slots.pop() {
            self.slot_states[(id - 1) as usize] = SlotState::default();
            id
        } else {
            let id = self.next_slot_state_id;
            self.next_slot_state_id += 1;
            self.slot_states.push(SlotState::default());
            id
        }
    }

    fn free_slot(&mut self, slot_id: SlotStateId) {
        if (slot_id - 1) as usize >= self.slot_states.len() {
            panic!("Invalid slot ID");
        }
        self.slot_states[(slot_id - 1) as usize] = SlotState::default();
        self.free_slots.push(slot_id);
    }

    fn get_slot_state(&self, slot_id: SlotStateId) -> Option<&SlotState> {
        self.slot_states.get((slot_id - 1) as usize)
    }

    fn set_slot_state(&mut self, slot_id: SlotStateId, value: Value) {
        if (slot_id - 1) as usize >= self.slot_states.len() {
            panic!("Invalid slot ID");
        }
        let state = &mut self.slot_states[(slot_id - 1) as usize];
        state.present = true;
        state.value = value;
    }
}

// Runtime
struct Runtime {
    objects: HashMap<ObjectId, Object>,
    shapes: HashMap<ShapeId, Shape>,
    next_object_id: ObjectId,
    next_shape_id: ShapeId,
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            objects: HashMap::new(),
            shapes: HashMap::new(),
            next_object_id: 1,
            next_shape_id: 1,
        }
    }

    fn create_object(&mut self, name: String) -> ObjectId {
        let id = self.next_object_id;
        self.next_object_id += 1;
        let object = Object {
            id,
            name,
            slot_mapping: HashMap::new(),
            slot_states: Vec::new(),
            free_slots: Vec::new(),
            next_slot_state_id: 1,
        };

        self.objects.insert(id, object);
        id
    }

    fn create_shape(&mut self, name: String) -> ShapeId {
        let id = self.next_shape_id;
        self.next_shape_id += 1;

        let mut shape = Shape {
            id,
            name,
            defined_slots: HashSet::new(),
            slot_remapping: HashMap::new(),
            static_object_id: 0,
        };

        // Static singleton
        let static_singleton = self.create_object(format!("{}::static", shape.name));
        shape.static_object_id = static_singleton;

        self.shapes.insert(id, shape);
        id
    }

    fn define_slot_on_shape(&mut self, shape_id: ShapeId, num: u32, name: String, 
        kind: ValueKind, is_static: bool) -> SlotId {
        let slot = {
            let shape = self.get_shape_mut(shape_id);
            shape.define_slot(num, name, kind, is_static)
        };

        if is_static {
            let static_object_id = self.get_shape(shape_id).static_object_id;
            self.attach_slot(static_object_id, slot.clone());
        }
        
        slot
    }

    fn define_remapping_on_shape(&mut self, shape_id: ShapeId, from: &SlotId, to: &SlotId) {
        let shape = self.get_shape_mut(shape_id);
        shape.remap_slot(from, to);
    }

    fn attach_slot(&mut self, object_id: ObjectId, slot: SlotId) {
        let shape = self.get_shape(slot.shape_id);
        let target_slot;
        
        // Find default remapping if exists
        if let Some(remapped_slot) = shape.slot_remapping.get(&slot) {
            target_slot = remapped_slot.clone();
        } else {
            target_slot = slot.clone();
        }

        let object = self.get_object_mut(object_id);

        if object.slot_mapping.contains_key(&slot) {
            panic!("Slot already attached to object");
        }

        // Check for present slot mappings
        let state_id;
        if target_slot != slot && object.slot_mapping.contains_key(&target_slot) {
            state_id = *object.slot_mapping.get(&target_slot).unwrap();
        } else {
            state_id = object.allocate_slot();

            // If different target, also map it
            if target_slot != slot {
                object.slot_mapping.insert(target_slot.clone(), state_id);
            }
        }

        object.slot_mapping.insert(slot.clone(), state_id);
    }

    fn detach_slot(&mut self, object_id: ObjectId, slot: SlotId) {
        let object = self.get_object_mut(object_id);

        if let Some(state_id) = object.slot_mapping.remove(&slot) {
            // Check if any other slot maps to the same state
            let still_mapped = object.slot_mapping.values().any(|&id| id == state_id);
            if !still_mapped {
                object.free_slot(state_id);
            }
        } else {
            panic!("Slot not attached to object");
        }
    }

    fn attach_shape(&mut self, object_id: ObjectId, shape_id: ShapeId) {
        let slots: Vec<SlotId> = self.get_shape_mut(shape_id)
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        for slot in slots {
            self.attach_slot(object_id, slot);
        }
    }

    fn detach_shape(&mut self, object_id: ObjectId, shape_id: ShapeId) {
        let slots: Vec<SlotId> = self.get_shape_mut(shape_id)
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        for slot in slots {
            self.detach_slot(object_id, slot);
        }
    }

    fn get_object(&self, id: ObjectId) -> &Object {
        self.objects.get(&id).expect("Object not found")
    }

    fn get_object_mut(&mut self, id: ObjectId) -> &mut Object {
        self.objects.get_mut(&id).expect("Object not found")
    }

    fn get_shape(&self, id: ShapeId) -> &Shape {
        self.shapes.get(&id).expect("Shape not found")
    }

    fn get_shape_mut(&mut self, id: ShapeId) -> &mut Shape {
        self.shapes.get_mut(&id).expect("Shape not found")
    }

    fn is_shape(&self, object_id: ObjectId, shape_id: ShapeId) -> bool {
        let object = self.get_object(object_id);
        let shape = self.get_shape(shape_id);

        shape.defined_slots.iter().all(|slot| {
            if slot.is_static {
                let static_object = self.get_object(shape.static_object_id);
                static_object.slot_mapping.contains_key(slot)
            } else {
                object.slot_mapping.contains_key(slot)
            }
        })
    }

    fn get_slot_value(&self, object_id: ObjectId, slot: &SlotId) -> Option<&Value> {
        let object = self.get_object(object_id);
        if let Some(state_id) = object.slot_mapping.get(slot) {
            if let Some(state) = object.get_slot_state(*state_id) {
                if state.present {
                    return Some(&state.value);
                }
            }
        }
        None
    }

    fn set_slot_value(&mut self, object_id: ObjectId, slot: &SlotId, value: Value) {
        let object = self.get_object_mut(object_id);
        if let Some(state_id) = object.slot_mapping.get(slot) {
            if let Some(state) = object.get_slot_state(*state_id) {
                if state.sealed {
                    panic!("Slot is sealed and cannot be modified");
                }
                object.set_slot_state(*state_id, value);
            } else {
                panic!("Invalid slot state ID");
            }
        } else {
            panic!("Slot not attached to object");
        }
    }


}

fn main() {
    println!("Hello, world!");
}
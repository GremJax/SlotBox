use std::collections::{HashMap, HashSet};

pub mod ast;
pub mod tokenizer;

// Runtime Value
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum ValueKind {
    Int32,
    Float32,
    uInt32,
    Bool,
    String,
    ObjectId(ShapeId),
    Array(Box<ValueKind>),
    SlotId(Box<ValueKind>),
    Pointer(ObjectId, Box<SlotId>),
    Function,
    None
}

#[derive(Default, Debug, Clone)]
enum PrimitiveValue {
    Int32(i32),
    Float32(f32),
    uInt32(u32),
    Bool(bool),
    String(String),
    ObjectId(ShapeId, ObjectId),
    SlotId(SlotId),
    Pointer(ObjectId, SlotId),
    Function(Function),
    #[default] None,
}

impl PrimitiveValue {
    fn is(&self, kind: &ValueKind) -> bool { self.kind() == *kind }

    fn kind(&self) -> ValueKind {
        match self {
            PrimitiveValue::Int32(_) => ValueKind::Int32,
            PrimitiveValue::Float32(_) => ValueKind::Float32,
            PrimitiveValue::uInt32(_) => ValueKind::uInt32,
            PrimitiveValue::Bool(_) => ValueKind::Bool,
            PrimitiveValue::String(_) => ValueKind::String,
            PrimitiveValue::ObjectId(shape_id, _) => ValueKind::ObjectId(*shape_id),
            PrimitiveValue::SlotId(s) => ValueKind::SlotId(Box::new(s.value_type.clone())),
            PrimitiveValue::Pointer(object_id, slot) => ValueKind::Pointer(*object_id, Box::new(slot.clone())),
            PrimitiveValue::Function(_) => ValueKind::Function,
            PrimitiveValue::None => ValueKind::None,
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
#[derive(Debug)]
enum Value {
    Single(PrimitiveValue),
    Array(Vec<Box<Value>>),
    Empty,
}

impl Default for Value {
    fn default() -> Self {
        Value::Single(PrimitiveValue::default())
    }
}

type SlotStateId = u32;

#[derive(Default)]
struct SlotState {
    sealed: bool,
    storage: Value,
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
            name: format!("{}.{}", self.name, name),
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
        state.storage = value;
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

        println!("Created object with ID {}, '{}'", id, object.name);
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

        println!("Created shape with ID {}, '{}'", id, shape.name);

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
        
        println!("Defined slot {}", slot.name);
        slot
    }

    fn define_remapping_on_shape(&mut self, shape_id: ShapeId, from: &SlotId, to: &SlotId) {
        let shape = self.get_shape_mut(shape_id);
        shape.remap_slot(from, to);
        println!("Mapping {} -> {} in shape {}", from.name, to.name, shape.name);
    }

    fn attach_slot(&mut self, object_id: ObjectId, slot: SlotId) {
        let shape = self.get_shape(slot.shape_id);
        let target_slot;
        
        // Find default remapping if exists
        if let Some(remapped_slot) = shape.slot_remapping.get(&slot) {
            target_slot = remapped_slot.clone();
            println!("- Will remap {} -> {}", slot.name, target_slot.name);
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
            println!("- Mapping {} to existing local slot {} (shared with {})", slot.name, state_id, target_slot.name);

        } else {
            state_id = object.allocate_slot();
            println!("- Allocated local slot {} for {}", state_id, slot.name);

            // If different target, also map it
            if target_slot != slot {
                object.slot_mapping.insert(target_slot.clone(), state_id);
                println!("- Also mapping {} to same local slot {}", target_slot.name, state_id);
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
        let shape = self.get_shape_mut(shape_id);
        let slots: Vec<SlotId> = shape
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        println!("Attaching shape {} to object {}", shape.name, object_id);
        
        for slot in slots {
            self.attach_slot(object_id, slot);
        }
    }

    fn detach_shape(&mut self, object_id: ObjectId, shape_id: ShapeId) {
        let shape = self.get_shape_mut(shape_id);
        let slots: Vec<SlotId> = shape
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        println!("Detaching shape {} from object {}", shape.name, object_id);
        
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
                return Some(&state.storage);
            }
        }
        None
    }

    fn get_slot_value_mut(&mut self, object_id: ObjectId, slot: &SlotId) -> Option<&mut Value> {
        let object = self.get_object_mut(object_id);
        if let Some(state_id) = object.slot_mapping.get(slot) {
            let state_id_copy = *state_id;
            if ((state_id_copy - 1) as usize) < object.slot_states.len() {
                return Some(&mut object.slot_states[(state_id_copy - 1) as usize].storage);
            }
        }
        None
    }

    fn get_slot_value_array_element(&self, object_id: ObjectId, slot: &SlotId, index: usize) -> Option<&Value> {
        if let Some(Value::Array(arr)) = self.get_slot_value(object_id, slot) {
            return arr.get(index).map(|boxed| boxed.as_ref());
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

    fn set_slot_value_primitive(&mut self, object_id: ObjectId, slot: &SlotId, value: PrimitiveValue) {
        self.set_slot_value(object_id, slot, Value::Single(value));
    }

    fn set_slot_value_array(&mut self, object_id: ObjectId, slot: &SlotId, values: Vec<Value>) {
        self.set_slot_value(object_id, slot, Value::Array(values.into_iter().map(Box::new).collect()));
    }
    fn set_slot_value_array_element(&mut self, object_id: ObjectId, slot: &SlotId, index: usize, value: Value) {
        if let Some(Value::Array(arr)) = self.get_slot_value_mut(object_id, slot) {
            if index < arr.len() {
                arr[index] = Box::new(value);
            } else {
                panic!("Array index out of bounds");
            }
        } else {
            panic!("Slot does not contain an array");
        }
    }

    fn print_object(&self, object_id: ObjectId) {
        let object = self.get_object(object_id);
        println!("Object {}: '{}'", object.id, object.name);

        for state_id in 1..=object.slot_states.len() as SlotStateId{
            if let Some(state) = object.get_slot_state(state_id) {
                
                print!("Local slot {}: ", state_id);
                println!("{:?}", &state.storage);

                let mapped_slots: Vec<String> = object.slot_mapping.iter()
                    .filter(|(_, id)| **id == state_id)
                    .map(|(slot, _)| slot.name.clone())
                    .collect();
                println!("  Mapped slots: {}", mapped_slots.join(", "));

            }   
        }
    }

}

fn print_hello(params: FunctionParams) -> Value {
    println!("Hello from native function! Caller: {}", params.caller);
    Value::Single(PrimitiveValue::None)
}

fn main() {
    println!("Hello, world!");

    let mut runtime = Runtime::new();

    // Vector Shape
    let vector_shape = runtime.create_shape("Vector".to_string());
    let vec_x = runtime.define_slot_on_shape(vector_shape, 1, "x".to_string(), ValueKind::Int32, false);
    let vec_y = runtime.define_slot_on_shape(vector_shape, 2, "y".to_string(), ValueKind::Int32, false);
    
    let static_field = runtime.define_slot_on_shape(vector_shape, 3, "static_field".to_string(), ValueKind::String, true);
    runtime.set_slot_value(runtime.get_shape(vector_shape).static_object_id, &static_field, Value::Single(PrimitiveValue::String("I am static".to_string())));
    println!();

    // Position Shape
    let position_shape = runtime.create_shape("Position".to_string());
    let pos_x = runtime.define_slot_on_shape(position_shape, 1, "x".to_string(), ValueKind::Int32, false);
    let pos_y = runtime.define_slot_on_shape(position_shape, 2, "y".to_string(), ValueKind::Int32, false);
    runtime.define_remapping_on_shape(position_shape, &pos_x, &vec_x);
    runtime.define_remapping_on_shape(position_shape, &pos_y, &vec_y);
    println!();

    // Create object and attach shape
    let obj1 = runtime.create_object("Object1".to_string());
    runtime.attach_shape(obj1, vector_shape);
    runtime.print_object(obj1);
    println!();

    // Set values
    runtime.set_slot_value_primitive(obj1, &vec_x, PrimitiveValue::Int32(10));
    runtime.set_slot_value_primitive(obj1, &vec_y, PrimitiveValue::Int32(20));
    runtime.print_object(obj1);
    println!();

    // Attach position
    runtime.attach_shape(obj1, position_shape);
    runtime.print_object(obj1);
    println!();

    // Modify via position slots
    runtime.set_slot_value_primitive(obj1, &pos_x, PrimitiveValue::Int32(30));
    runtime.set_slot_value_primitive(obj1, &pos_y, PrimitiveValue::Int32(40));
    runtime.print_object(obj1);
    println!();

    // Detach
    runtime.detach_shape(obj1, vector_shape);
    runtime.print_object(obj1);
    println!();

    // Deallocate
    runtime.detach_shape(obj1, position_shape);
    runtime.print_object(obj1);
    println!();

    // Print static field
    let static_value = runtime.get_slot_value(runtime.get_shape(vector_shape).static_object_id, &static_field);
    println!("Static field value: {:?}", static_value);

    // Execute static function
    let static_func = runtime.define_slot_on_shape(vector_shape, 4, "static_func".to_string(), ValueKind::Function, true);
    runtime.set_slot_value_primitive(runtime.get_shape(vector_shape).static_object_id, &static_func, PrimitiveValue::Function(Function {
        self_type: vector_shape,
        input_types: vec![],
        output_type: ValueKind::None,
        func: print_hello,
    }));
    
    if let Some(Value::Single(PrimitiveValue::Function(func))) = runtime.get_slot_value(runtime.get_shape(vector_shape).static_object_id, &static_func) {
        let params = FunctionParams {
            caller: obj1,
            inputs: vec![],
        };
        (func.func)(params);
    }

    // Is shape checks
    let obj2 = runtime.create_object("Object2".to_string());
    runtime.attach_shape(obj2, vector_shape);
    runtime.print_object(obj2);
    println!();

    println!("Is Object1 a Vector? {}", runtime.is_shape(obj1, vector_shape));
    println!("Is Object2 a Vector? {}", runtime.is_shape(obj2, vector_shape));

    // Arrays
    let array_slot = runtime.define_slot_on_shape(vector_shape, 5, "array_field".to_string(), ValueKind::Array(Box::new(ValueKind::Int32)), false);
    runtime.attach_shape(obj1, vector_shape);
    runtime.set_slot_value_array(obj1, &array_slot, vec![Value::Single(PrimitiveValue::Int32(1)), Value::Single(PrimitiveValue::Int32(2)), Value::Single(PrimitiveValue::Int32(3))]);
    runtime.print_object(obj1);

    runtime.set_slot_value_array_element(obj1, &array_slot, 1, Value::Single(PrimitiveValue::Int32(42)));
    runtime.print_object(obj1);

}
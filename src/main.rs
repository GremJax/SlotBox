use std::{collections::{HashMap, HashSet}, fs};

pub mod parser;
pub mod tokenizer;
pub mod executor;

// Runtime Value
#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Int32,
    Float32,
    uInt32,
    Bool,
    String,
    ObjectId(ShapeId),
    Array(Box<ValueKind>),
    Azimuth(Box<ValueKind>),
    Pointer(ObjectId, Box<Azimuth>),
    Function,
    Generic(GenericId),
    #[default] None
}

type GenericId = u8;

#[derive(Default, Debug, Clone)]
pub enum PrimitiveValue {
    Int32(i32),
    Float32(f32),
    uInt32(u32),
    Bool(bool),
    String(String),
    ObjectId(ShapeId, ObjectId),
    Azimuth(AzimuthState),
    Pointer(ObjectId, Azimuth),
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
            PrimitiveValue::Azimuth(s) => ValueKind::Azimuth(Box::new(s.value_type.clone())),
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
pub struct Function {
    self_type: ShapeId,
    input_types: Vec<ValueKind>,
    output_type: ValueKind,
    func: NativeFn,
}

// Slot Id
pub type AzimuthId = u32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Azimuth {
    shape_id: ShapeId,
    slot_num: AzimuthId,
    name: String,
    value_type: ValueKind,
    is_static: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AzimuthState {
    azimuth: Azimuth,
    value_type: ValueKind,
}

pub struct Mapping {
    pub from_slot: Azimuth,
    pub to_slot: Azimuth,
}

// Runtime Slot state
#[derive(Default, Debug, Clone)]
pub enum Value {
    Single(PrimitiveValue),
    Array(Vec<Box<Value>>),
    #[default] Empty,
}

type SlotStateId = u32;

#[derive(Default)]
struct SlotState {
    sealed: bool,
    storage: Value,
}

// Shape
pub type ShapeId = u32;

pub struct Shape {
    id: ShapeId,
    name: String,
    
    num_generics: u8,
    defined_slots: HashSet<Azimuth>,
    slot_remapping: HashMap<Azimuth, Azimuth>,
    static_object_id: ObjectId,
}

impl Shape {
    fn define_slot(&mut self, num: AzimuthId, name: String, 
        kind: ValueKind, is_static: bool) -> Azimuth {
        let slot = Azimuth {
            shape_id: self.id,
            slot_num: num,
            name: format!("{}.{}", self.name, name),
            value_type: kind,
            is_static,
        };
        self.defined_slots.insert(slot.clone());
        slot
    }

    fn remap_slot(&mut self, from: &Azimuth, to: &Azimuth) {
        self.slot_remapping.insert(from.clone(), to.clone());
    }
}

// Object
pub type ObjectId = u32;

pub struct Object {
    id: ObjectId,
    name: String,

    slot_mapping: Vec<(AzimuthState, SlotStateId)>,
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

    fn has_azimuth(&self, azimuth: Azimuth) -> bool {
        for (azimuth_state, _) in self.slot_mapping.clone() {
            if azimuth_state.azimuth == azimuth { return true }
        }
        false
    }

    fn remove_azimuth(&mut self, azimuth: Azimuth)  -> Option<SlotStateId> {
        for i in (0..self.slot_mapping.len()).rev() {
            if self.slot_mapping[i].0.azimuth == azimuth {
                let id = self.slot_mapping.remove(i).1;
                return Some(id);
            }
        }
        None
    }

    fn get_slot_state_id(&self, azimuth: Azimuth) -> Option<SlotStateId> {
        for (azimuth_state, slot_state_id) in self.slot_mapping.clone() {
            if azimuth_state.azimuth == azimuth { return Some(slot_state_id) }
        }
        None
    }
}

// Runtime
pub struct Runtime {
    objects: HashMap<ObjectId, Object>,
    shapes: HashMap<ShapeId, Shape>,    
    next_object_id: ObjectId,
    next_shape_id: ShapeId,
    next_azimuth_id: AzimuthId,
    global: Object
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            objects: HashMap::new(),
            shapes: HashMap::new(),
            next_object_id: 1,
            next_shape_id: 1,
            next_azimuth_id: 1,
            global: Object {
                id: 0,
                name: "global".to_string(),
                slot_mapping: Vec::new(),
                slot_states: Vec::new(),
                free_slots: Vec::new(),
                next_slot_state_id: 1,
            }
        }
    }

    fn create_object(&mut self, name: String) -> ObjectId {
        let id = self.next_object_id;
        self.next_object_id += 1;
        let object = Object {
            id,
            name,
            slot_mapping: Vec::new(),
            slot_states: Vec::new(),
            free_slots: Vec::new(),
            next_slot_state_id: 1,
        };

        println!("Created object with ID {}, '{}'", id, object.name);
        self.objects.insert(id, object);
        id
    }

    fn create_shape(&mut self, name: String) -> ShapeId {
        self.create_shape_with_generics(name, 0)
    }

    fn create_shape_with_generics(&mut self, name: String, num_generics: u8) -> ShapeId {
        let id = self.next_shape_id;
        self.next_shape_id += 1;

        let shape = Shape {
            id,
            name,
            num_generics,
            defined_slots: HashSet::new(),
            slot_remapping: HashMap::new(),
            static_object_id: 0,
        };

        println!("Created shape with ID {}, '{}'", id, shape.name);

        self.shapes.insert(id, shape);
        id
    }

    fn define_slot_on_shape(&mut self, shape_id: ShapeId, name: String, 
        kind: ValueKind, is_static: bool) -> Azimuth {
            
        let next_num = self.next_azimuth_id.clone();
        self.next_azimuth_id += 1;

        let slot = {
            let shape = self.get_shape_mut(shape_id);
            shape.define_slot(next_num, name, kind, is_static)
        };

        if is_static {
            // Define shape and id
            let (shape_name, static_object_id) = {
                let shape = self.get_shape_mut(shape_id);
                (shape.name.clone(), shape.static_object_id)
            };

            // Create static singleton if does not exist
            if static_object_id == 0 {
                let static_singleton = self.create_object(format!("{}::static", shape_name));
                let shape = self.get_shape_mut(shape_id);
                shape.static_object_id = static_singleton;
            }

            self.attach_slot(static_object_id, slot.clone());
        }
        
        println!("Defined slot {}", slot.name);
        slot
    }

    fn define_remapping_on_shape(&mut self, shape_id: ShapeId, from: &Azimuth, to: &Azimuth) {
        let shape = self.get_shape_mut(shape_id);
        shape.remap_slot(from, to);
        println!("Mapping {} -> {} in shape {}", from.name, to.name, shape.name);
    }

    fn attach_slot(&mut self, object_id: ObjectId, slot: Azimuth) {
        self.attach_slot_remap(object_id, slot, None);
    }

    fn attach_slot_generic(&mut self, object_id: ObjectId, slot: Azimuth, generic: Option<ValueKind>) {
        self.attach_slot_remap_generic(object_id, slot, None, generic);
    }
    
    fn attach_slot_remap(&mut self, object_id: ObjectId, slot: Azimuth, remap: Option<Azimuth>) {
        self.attach_slot_remap_generic(object_id, slot, remap, None);
    }

    fn attach_slot_remap_generic(&mut self, object_id: ObjectId, slot: Azimuth, remap: Option<Azimuth>, generic: Option<ValueKind>) {
        let shape = self.get_shape(slot.shape_id);
        let target_slot;
        
        if let Some(remap_slot) = remap {
            target_slot = remap_slot.clone();
            println!("- Will remap {} -> {} (explicit)", slot.name, target_slot.name);
        }
        // Find default remapping if exists
        else if let Some(remapped_slot) = shape.slot_remapping.get(&slot) {
            target_slot = remapped_slot.clone();
            println!("- Will remap {} -> {}", slot.name, target_slot.name);
        } else {
            target_slot = slot.clone();
        }

        let object = self.get_object_mut(object_id);

        if object.has_azimuth(slot.clone()) {
            panic!("Slot already attached to object");
        }

        // Check for present slot mappings
        let state_id;
        let target_slot_name = target_slot.name.clone();
        if target_slot != slot && object.has_azimuth(target_slot.clone()) {
            state_id = object.get_slot_state_id(target_slot.clone()).unwrap();
            println!("- Mapping {} to existing local slot {} (shared with {})", slot.name, state_id, target_slot_name);

        } else {
            state_id = object.allocate_slot();
            println!("- Allocated local slot {} for {}", state_id, slot.name);

        // If different target, also map it
            if target_slot != slot {
                let azimuth_state = AzimuthState{azimuth: target_slot.clone(), value_type: generic.clone().unwrap_or(target_slot.value_type.clone())};
                object.slot_mapping.push((azimuth_state, state_id));
                println!("- Also mapping {} to same local slot {}", target_slot_name, state_id);
            }
        }

        let azimuth_state = AzimuthState{azimuth: slot.clone(), value_type: generic.unwrap_or(slot.value_type)};
        object.slot_mapping.push((azimuth_state, state_id));
    }

    fn detach_slot(&mut self, object_id: ObjectId, slot: Azimuth) {
        let object = self.get_object_mut(object_id);

        if let Some(state_id) = object.remove_azimuth(slot) {
            // Check if any other slot maps to the same state
            let still_mapped = object.slot_mapping.iter().any(|(_,id)| *id == state_id);
            if !still_mapped {
                object.free_slot(state_id);
            }
        } else {
            panic!("Slot not attached to object");
        }
    }

    fn remap_slot(&mut self, object_id: ObjectId, to: &Azimuth, from: &Azimuth) {
        let object = self.get_object_mut(object_id);

        if let Some(to_id) = object.get_slot_state_id(to.clone()) {
            
            let azimuth_state = AzimuthState{azimuth: from.clone(), value_type: to.value_type.clone()};
            object.slot_mapping.push((azimuth_state, to_id));
        } else {
            panic!("Target slot {} not attached to object", to.name);
        }
    }

    fn attach_shape(&mut self, object_id: ObjectId, shape_id: ShapeId, generics: Vec<ValueKind>) {
        let shape = self.get_shape_mut(shape_id);
        if(shape.num_generics != generics.len() as u8) {
            panic!("Invalid number of generic arguments: {:?}", generics);
        }

        let slots: Vec<Azimuth> = shape
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        println!("Attaching shape {} to object {}", shape.name, object_id);
        
        for slot in slots {
            let mut generic_type = None;
            if let ValueKind::Generic(generic) = slot.value_type {
                if let Some(k) = generics.get(generic as usize) {
                    generic_type = Some(k.clone());
                }
            }

            self.attach_slot_generic(object_id, slot, generic_type);
        }
    }

    fn detach_shape(&mut self, object_id: ObjectId, shape_id: ShapeId) {
        let shape = self.get_shape_mut(shape_id);
        let slots: Vec<Azimuth> = shape
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

    fn attach_shape_with_remap(&mut self, object_id: ObjectId, shape_id: ShapeId, remap: &Vec<Mapping>, generics: Vec<ValueKind>) {
        let shape = self.get_shape_mut(shape_id);
        if(shape.num_generics != generics.len() as u8) {
            panic!("Invalid number of generic arguments: {:?}", generics);
        }
        
        let slots: Vec<Azimuth> = shape
            .defined_slots
            .iter()
            .filter(|slot| !slot.is_static)
            .cloned()
            .collect();
        
        println!("Attaching shape {} to object {} with remapping", shape.name, object_id);
        
        for mut slot in slots {
            let mut generic_type = None;
            if let ValueKind::Generic(generic) = slot.value_type {
                if let Some(k) = generics.get(generic as usize) {
                    generic_type = Some(k.clone());
                }
            }

            if let Some(remapped) = remap.iter().find(|mapping| mapping.from_slot == slot).map(|mapping| mapping.to_slot.clone()) {
                self.attach_slot_remap_generic(object_id, slot, Some(remapped), generic_type);
            } else {
                self.attach_slot_generic(object_id, slot, generic_type);
            };
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
                static_object.has_azimuth(slot.clone())
            } else {
                object.has_azimuth(slot.clone())
            }
        })
    }

    fn get_slot_value(&self, object_id: ObjectId, slot: &Azimuth) -> Option<&Value> {
        let object = self.get_object(object_id);
        if let Some(state_id) = object.get_slot_state_id(slot.clone()) {
            if let Some(state) = object.get_slot_state(state_id) {
                return Some(&state.storage);
            }
        }
        None
    }

    fn get_slot_value_mut(&mut self, object_id: ObjectId, slot: &Azimuth) -> Option<&mut Value> {
        let object = self.get_object_mut(object_id);
        if let Some(state_id) = object.get_slot_state_id(slot.clone()) {
            if ((state_id - 1) as usize) < object.slot_states.len() {
                return Some(&mut object.slot_states[(state_id - 1) as usize].storage);
            }
        }
        None
    }

    fn get_slot_value_array_element(&self, object_id: ObjectId, slot: &Azimuth, index: usize) -> Option<&Value> {
        if let Some(Value::Array(arr)) = self.get_slot_value(object_id, slot) {
            return arr.get(index).map(|boxed| boxed.as_ref());
        }
        None
    }

    fn set_slot_value(&mut self, object_id: ObjectId, slot: &Azimuth, value: Value) {
        let object = self.get_object_mut(object_id);
        if let Some(state_id) = object.get_slot_state_id(slot.clone()) {
            if let Some(state) = object.get_slot_state(state_id) {
                if state.sealed {
                    panic!("Slot is sealed and cannot be modified");
                }
                object.set_slot_state(state_id, value);
            } else {
                panic!("Invalid slot state ID");
            }
        } else {
            panic!("Slot not attached to object");
        }
    }

    fn set_slot_value_primitive(&mut self, object_id: ObjectId, slot: &Azimuth, value: PrimitiveValue) {
        self.set_slot_value(object_id, slot, Value::Single(value));
    }

    fn set_slot_value_array(&mut self, object_id: ObjectId, slot: &Azimuth, values: Vec<Value>) {
        self.set_slot_value(object_id, slot, Value::Array(values.into_iter().map(Box::new).collect()));
    }
    fn set_slot_value_array_element(&mut self, object_id: ObjectId, slot: &Azimuth, index: usize, value: Value) {
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
        println!("\nObject {}: '{}'", object.id, object.name);

        for state_id in 1..=object.slot_states.len() as SlotStateId{
            if let Some(state) = object.get_slot_state(state_id) {
                
                print!("Local slot {}: ", state_id);
                println!("{:?}", &state.storage);

                let mapped_slots: Vec<String> = object.slot_mapping.iter()
                    .filter(|(_, id)| *id == state_id)
                    .map(|(slot, _)| slot.azimuth.name.clone())
                    .collect();
                println!("  Mapped slots: {}", mapped_slots.join(", "));

            }   
        }
        println!();
    }

}

fn print_hello(params: FunctionParams) -> Value {
    println!("Hello from native function! Caller: {}", params.caller);
    Value::Single(PrimitiveValue::None)
}

fn read_source(path: &str) -> Result<String, std::io::Error> {
    fs::read_to_string(path)
}

fn main() {
    let mut runtime = Runtime::new();

    let source = read_source("/workspaces/SlotBox/src/lang.az");
    let tokens = tokenizer::tokenize(&source.unwrap_or_else(|_| panic!("Failed to read source file")));
    let ast = parser::parse(tokens);
    executor::execute(ast, &mut runtime, &mut HashMap::new());
}
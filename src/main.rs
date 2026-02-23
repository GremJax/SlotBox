use std::{collections::{HashMap, HashSet}, fs};

use crate::{
    analyzer::{AzimuthInfo, ObjectInfo, ResolvedShapeExpression, ResolvedStatement, ShapeInfo, Symbol, LocalId}, 
    executor::{RuntimeError, ShapeInstance}, parser::ShapeExpression
};

pub mod parser;
pub mod tokenizer;
pub mod executor;
pub mod analyzer;

// Runtime Value
#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Int32,
    Float32,
    uInt32,
    Bool,
    String,
    Shape(ShapeInstance),
    Array(Box<ValueKind>),
    Azimuth(Box<ValueKind>),
    Option(Box<ValueKind>),
    Object(ObjectId, Box<ValueKind>),
    Pointer(ObjectId, AzimuthId, Box<ValueKind>),
    ArrayElement(Box<ValueKind>),
    Function(Box<FunctionInfo>),
    Generic(GenericId),
    #[default] None
}

impl ValueKind {

    fn is_assignable_from(&self, other: ValueKind) -> bool {
        match self {
            ValueKind::Int32 => other == ValueKind::Int32,
            ValueKind::Float32 => other == ValueKind::Float32,
            ValueKind::uInt32 => other == ValueKind::uInt32,
            ValueKind::Bool => other == ValueKind::Bool,
            ValueKind::String => other == ValueKind::String,
            ValueKind::Shape(k) => other == ValueKind::Shape(k.clone()),
            ValueKind::Array(k) => match other {
                ValueKind::Array(other_k) => k.is_assignable_from(*other_k),
                _ => false,
            },
            ValueKind::Azimuth(k) => match other {
                ValueKind::Azimuth(other_k) => k.is_assignable_from(*other_k),
                _ => false,
            },
            ValueKind::Option(k) => k.is_assignable_from(other),
            ValueKind::Object(_, k) => k.is_assignable_from(other),
            ValueKind::Pointer(_, _, k) => k.is_assignable_from(other),
            ValueKind::ArrayElement(k) => k.is_assignable_from(other),
            ValueKind::Function(info) => other == ValueKind::Function(info.clone()),
            ValueKind::Generic(_) => true,
            ValueKind::None => other == ValueKind::None,
        }
    }

}

type GenericId = u8;

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub enum PrimitiveValue {
    Int32(i32),
    uInt32(u32),
    Bool(bool),
    String(String),
    //ObjectId(ShapeInstance, ObjectId),
    Azimuth(AzimuthState),
    Object(ObjectId, ValueKind),
    Pointer(ObjectId, AzimuthId, ValueKind),
    ArrayElement(ObjectId, AzimuthId, ValueKind, usize),
    Function(Box<Function>),
    #[default] None,
}

impl PrimitiveValue {
    fn is(&self, kind: &ValueKind) -> bool { self.kind() == *kind }

    fn kind(&self) -> ValueKind {
        match self {
            PrimitiveValue::Int32(_) => ValueKind::Int32,
            PrimitiveValue::uInt32(_) => ValueKind::uInt32,
            PrimitiveValue::Bool(_) => ValueKind::Bool,
            PrimitiveValue::String(_) => ValueKind::String,
            //PrimitiveValue::ObjectId(shape_id, _) => ValueKind::Shape(shape_id.clone()),
            PrimitiveValue::Azimuth(s) => ValueKind::Azimuth(Box::new(s.value_type.clone())),
            PrimitiveValue::Object(_, kind) => kind.clone(),
            PrimitiveValue::Pointer(_, _, kind) => kind.clone(),
            PrimitiveValue::ArrayElement(_, _, kind, _) => kind.clone(),
            PrimitiveValue::Function(func) => 
                ValueKind::Function(Box::new(FunctionInfo{
                    has_self:func.has_self,
                    id:func.id, 
                    output_type:func.output_type.clone(),
                    input_types:func.input_types.clone()
                })),
            PrimitiveValue::None => ValueKind::None,
        }
    }
}

impl std::fmt::Display for PrimitiveValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveValue::Int32(val) =>
                write!(f, "{}", val),
            PrimitiveValue::Bool(val) =>
                write!(f, "{}", val),
            PrimitiveValue::String(val) =>
                write!(f, "{}", val),
            PrimitiveValue::Object(id, shape) =>
                write!(f, "Obj{}: {:?}", id, shape),
            PrimitiveValue::Pointer(id, az, shape) =>
                write!(f, "Obj{}.{}: {:?}", id, az, shape),
            val => 
                write!(f, "{:?}", val),
        }
    }
}

// Native Function
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub id: AzimuthId,
    pub has_self:bool,
    pub input_types: Vec<ValueKind>,
    pub output_type: ValueKind,
    pub func: ResolvedStatement,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionInfo {
    pub id: AzimuthId,
    pub input_types: Vec<ValueKind>,
    pub output_type: ValueKind,
    pub has_self: bool,
}

// Slot Id
pub type AzimuthId = u32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Azimuth {
    shape_id: ShapeId,
    id: AzimuthId,
    name: String,
    value_type: ValueKind,
    is_static: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AzimuthState {
    azimuth: AzimuthId,
    value_type: ValueKind,
}

#[derive(Default, Debug, Clone)]
pub struct Mapping {
    pub from: AzimuthId,
    pub to: AzimuthId,
}

// Runtime Slot state
#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Single(PrimitiveValue),
    Array(Vec<Box<Value>>, ValueKind),
    #[default] Empty,
}

impl Value {
    fn kind(&self) -> ValueKind {
        match self {
            Value::Single(k) => k.kind(),
            Value::Array(_, value_type) => ValueKind::Array(Box::new(value_type.clone())),
            Value::Empty => ValueKind::None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Single(val) =>
                write!(f, "{}", val),
            Value::Array(val, _) =>
                write!(f, "{:?}", val),
            Value::Empty =>
                write!(f, "Empty"),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Single(PrimitiveValue::Bool(value))
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Single(PrimitiveValue::Int32(value))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Single(PrimitiveValue::String(value))
    }
}

type SlotStateId = u32;

#[derive(Default, Debug, Clone)]
struct SlotState {
    sealed: bool,
    storage: Value,
}

// Shape
pub type ShapeId = u32;
pub struct Shape {
    id: ShapeId,
    name: String,
    
    num_generics: u32,
    azimuths: Vec<AzimuthId>,
    def_mappings: HashMap<AzimuthId, AzimuthId>,
    static_object_id: ObjectId,
}

impl Shape {
    fn define_slot(&mut self, id: AzimuthId, name: String, 
        kind: ValueKind, is_static: bool) -> Azimuth {
        let slot = Azimuth {
            shape_id: self.id,
            id: id.clone(),
            name: format!("{}.{}", self.name, name),
            value_type: kind,
            is_static,
        };
        self.azimuths.push(id);
        slot
    }

    fn remap_slot(&mut self, from: AzimuthId, to: AzimuthId) {
        //if !to.value_type.is_assignable_from(from.value_type.clone()) { 
        //    panic!("Type mismatch: {:?} not assignable from {:?}", to.value_type, from.value_type); 
        //}
        
        self.def_mappings.insert(from.clone(), to.clone());
    }
}

// Object
pub type ObjectId = u32;

#[derive(Debug, Clone)]
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

    fn has_azimuth(&self, azimuth: AzimuthId) -> bool {
        for (azimuth_state, _) in self.slot_mapping.clone() {
            if azimuth_state.azimuth == azimuth { return true }
        }
        false
    }

    fn remove_azimuth(&mut self, azimuth: AzimuthId)  -> Option<SlotStateId> {
        for i in (0..self.slot_mapping.len()).rev() {
            if self.slot_mapping[i].0.azimuth == azimuth {
                let id = self.slot_mapping.remove(i).1;
                return Some(id);
            }
        }
        None
    }

    fn get_slot_state_id(&self, azimuth: AzimuthId) -> Option<SlotStateId> {
        for (azimuth_state, slot_state_id) in self.slot_mapping.clone() {
            if azimuth_state.azimuth == azimuth { return Some(slot_state_id) }
        }
        None
    }

    fn set_value(&mut self, azimuth:AzimuthId, value:Value) -> Result<Value, RuntimeError> {
        for (azimuth_state, slot_state_id) in self.slot_mapping.clone() {
            if azimuth_state.azimuth == azimuth {

                if !value.kind().is_assignable_from(azimuth_state.value_type.clone()) {
                    panic!("Type mismatch: {:?} not assignable from {:?}", value.kind(), azimuth_state.value_type);
                }

                // Slot found
                self.set_slot_state(slot_state_id, value.clone());
                return Ok(value);
            }
        }
        panic!("No such azimuth {:?} to assign to {:?}", azimuth, value);
    }
}

// Runtime
pub struct Runtime {
    shapes: Vec<Shape>,    
    azimuths: Vec<Azimuth>,
    objects: Vec<Object>,
    locals: Vec<Value>,
    next_object_id: ObjectId,
    next_shape_id: ShapeId,
    next_azimuth_id: AzimuthId,
}

impl Runtime {
    fn new() -> Self {
        let global = Object {
                id: 0,
                name: "global".to_string(),
                slot_mapping: Vec::new(),
                slot_states: Vec::new(),
                free_slots: Vec::new(),
                next_slot_state_id: 1,
            };
        let mut runtime = Runtime {
            shapes: Vec::new(),
            azimuths: Vec::new(),
            objects: Vec::new(),
            next_object_id: 1,
            next_shape_id: 0,
            next_azimuth_id: 0,
            locals: Vec::new(),
        };
        runtime.objects.push(global);
        runtime
    }

    fn create_object(&mut self, info: ObjectInfo) {
        let object = Object {
            id: info.id,
            name: info.name,
            slot_mapping: Vec::new(),
            slot_states: Vec::new(),
            free_slots: Vec::new(),
            next_slot_state_id: 1,
        };

        println!("Created object with ID {}, '{}'", object.id, object.name);
        self.objects.push(object);
    }

    fn create_shape(&mut self, info: ShapeInfo) {
        let static_object_id = match *info.static_id {
            Some(Symbol::Object(info)) => {
                let id = info.id.clone();
                self.create_object(info);
                id
            }
            _ => 0,   
        };

        let shape = Shape {
            id: info.id,
            name: info.name,
            azimuths: info.azimuths,
            def_mappings: HashMap::new(),
            static_object_id: static_object_id,
            num_generics: info.generics.len() as u32,
        };
        self.shapes.push(shape);
    }

    fn create_azimuth(&mut self, info: AzimuthInfo) {
        let name = info.name.clone();
        let id = info.id.clone();

        let azimuth = Azimuth {
            id: info.id,
            name: info.name,
            value_type: info.value_type.clone(),
            shape_id: info.shape_id,
            is_static: info.is_static,
        };
        self.azimuths.push(azimuth);

        if info.is_static {
            // Allocate static slot
            let static_object_id = self.get_shape(info.shape_id).static_object_id.clone();

            let object = self.get_object_mut(static_object_id);
            let az_state = AzimuthState{ azimuth: info.id.clone(), value_type: info.value_type };
            let state_id = object.allocate_slot();
            object.slot_mapping.push((az_state, state_id));

            match *info.default_value.clone() {
                Some(expr) => {
                    let evaluated: Value = executor::evaluate(self, expr).unwrap();
                    self.set_slot_value(static_object_id, info.id, evaluated);
                }
                _ => {}
            }
        }

        println!("Created azimuth with ID {}, '{}'", id, name);
    }

    fn define_remapping_on_shape(&mut self, shape_id: ShapeId, from: AzimuthId, to: AzimuthId) {
        let shape = self.get_shape_mut(shape_id);
        shape.remap_slot(from, to);
        println!("Mapping {} -> {} in shape {}", from, to, shape.name);
    }

    fn attach_slot(&mut self, object_id: ObjectId, slot: AzimuthId) {
        self.attach_slot_remap(object_id, slot, None);
    }

    fn attach_slot_generic(&mut self, object_id: ObjectId, slot: AzimuthId, generic: Option<ValueKind>) {
        self.attach_slot_remap_generic(object_id, slot, None, generic);
    }
    
    fn attach_slot_remap(&mut self, object_id: ObjectId, slot: AzimuthId, remap: Option<AzimuthId>) {
        self.attach_slot_remap_generic(object_id, slot, remap, None);
    }

    fn attach_slot_remap_generic(&mut self, object_id: ObjectId, azimuth_id: AzimuthId, remap: Option<AzimuthId>, generic: Option<ValueKind>) {
        let azimuth = self.get_azimuth(azimuth_id).clone();
        let shape = self.get_shape(azimuth.shape_id);
        let target_slot_id;
        
        if let Some(remap_id) = remap {
            let remap_slot = self.get_azimuth(remap_id);
            if !generic.clone().unwrap_or(azimuth.value_type.clone()).is_assignable_from(remap_slot.value_type.clone()) { 
                panic!("Type mismatch: {:?} not assignable from {:?}", azimuth.name, remap_slot.value_type); 
            }
            
            target_slot_id = remap_id;
            println!("- Will remap {} -> {} (explicit)", azimuth.name, remap_slot.name);
        }
        // Find default remapping if exists
        else if let Some(remapped_id) = shape.def_mappings.get(&azimuth_id) {
            let remapped_slot = self.get_azimuth(*remapped_id);
            if !generic.clone().unwrap_or(azimuth.value_type.clone()).is_assignable_from(remapped_slot.value_type.clone()) { 
                panic!("Type mismatch: {:?} not assignable from {:?}", azimuth.name, remapped_slot.value_type); 
            }

            target_slot_id = *remapped_id;
            println!("- Will remap {} -> {}", azimuth.name, remapped_slot.name);
        } else {

            // New slot
            target_slot_id = azimuth_id;
        }
        let target_slot = self.get_azimuth(target_slot_id).clone();

        //println!("{:?}", self.objects);
        let object = self.get_object_mut(object_id);

        if object.has_azimuth(azimuth_id) {
            panic!("Slot already attached to object");
        }

        // Check for present slot mappings
        let state_id;

        if target_slot_id != azimuth_id && object.has_azimuth(target_slot_id) {
            state_id = object.get_slot_state_id(target_slot_id).unwrap();
            println!("- Mapping {} to existing local slot {} (shared with {})", azimuth.name, state_id, target_slot.name);

        } else {
            state_id = object.allocate_slot();
            println!("- Allocated local slot {} for {}", state_id, azimuth.name);

        // If different target, also map it
            if target_slot_id != azimuth_id {
                let azimuth_state = AzimuthState{azimuth: target_slot_id, value_type: generic.clone().unwrap_or_else(||target_slot.value_type.clone())};
                object.slot_mapping.push((azimuth_state, state_id));
                println!("- Also mapping {} to same local slot {}", target_slot.name, state_id);
            }
        }

        let azimuth_state = AzimuthState{azimuth: azimuth_id, value_type: generic.unwrap_or(azimuth.value_type.clone())};
        object.slot_mapping.push((azimuth_state, state_id));
    }

    fn detach_slot(&mut self, object_id: ObjectId, slot: AzimuthId) {
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

    fn remap_slot(&mut self, object_id: ObjectId, to: AzimuthId, from: AzimuthId) {
        let to_azimuth = self.get_azimuth(to).clone();
        let object = self.get_object_mut(object_id);

        if let Some(to_id) = object.get_slot_state_id(to) {
            let azimuth_state = AzimuthState{azimuth: from.clone(), value_type: to_azimuth.value_type.clone()};
            object.slot_mapping.push((azimuth_state, to_id));
        } else {
            panic!("Target slot {} not attached to object", to_azimuth.name);
        }
    }

    fn attach_shape(&mut self, object_id: ObjectId, shape_id: ShapeInstance) {
        let (shape_name, azimuths, num_generics) = {
            let shape = self.get_shape(shape_id.id);

            if shape.num_generics != (shape_id.generics.len() as u32) {
                panic!("Invalid number of generic arguments: {:?}", shape_id.generics);
            }

            (shape.name.clone(), shape.azimuths.clone(), shape.num_generics)
        };

        println!("Attaching shape {} to object {}", shape_name, object_id);

        for id in azimuths {
            let azimuth = self.get_azimuth(id);
            if azimuth.is_static { continue; }

            let generic_type = match azimuth.value_type {
                ValueKind::Generic(generic) => {
                    shape_id.generics.get(generic as usize).cloned()
                }
                _ => None,
            };

            self.attach_slot_generic(object_id, id, generic_type);
        }
    }

    fn detach_shape(&mut self, object_id: ObjectId, shape_id: ShapeInstance) {
        let (shape_name, azimuths) = {
            let shape = self.get_shape(shape_id.id);
            (shape.name.clone(), shape.azimuths.clone())
        };

        println!("Detaching shape {} from object {}", shape_name, object_id);

        for azimuth in azimuths {
            self.detach_slot(object_id, azimuth);
        }
    }

    fn attach_shape_with_remap(&mut self, object_id: ObjectId, shape_id: ShapeInstance, remap: Vec<Mapping>,) {
        let (shape_name, azimuths, num_generics) = {
            let shape = self.get_shape(shape_id.id);

            if shape.num_generics != (shape_id.generics.len() as u32) {
                panic!("Invalid number of generic arguments: {:?}", shape_id.generics);
            }

            (shape.name.clone(), shape.azimuths.clone(), shape.num_generics)
        };

        println!(
            "Attaching shape {} to object {} with remapping",
            shape_name, object_id
        );

        for az_id in azimuths {
            let azimuth = self.get_azimuth(az_id);
            if azimuth.is_static { continue; }

            let generic_type = match azimuth.value_type {
                ValueKind::Generic(generic) => {
                    shape_id.generics.get(generic as usize).cloned()
                }
                _ => None,
            };

            if let Some(remapped) =
                remap.iter().find(|m| m.from == az_id).map(|m| m.to)
            {
                self.attach_slot_remap_generic(object_id, az_id, Some(remapped), generic_type);
            } else {
                self.attach_slot_generic(object_id, az_id, generic_type);
            }
        }
    }

    fn get_object(&self, id: ObjectId) -> &Object {
        self.objects.get(id as usize).expect(format!("Object {} not found", id).as_str())
    }

    fn get_object_mut(&mut self, id: ObjectId) -> &mut Object {
        self.objects.get_mut(id as usize).expect(format!("Object {} not found", id).as_str())
    }

    fn get_azimuth(&self, id: AzimuthId) -> &Azimuth {
        self.azimuths.get(id as usize).expect(format!("Azimuth {} not found", id).as_str())
    }

    fn get_azimuth_mut(&mut self, id: AzimuthId) -> &Azimuth {
        self.azimuths.get_mut(id as usize).expect(format!("Azimuth {} not found", id).as_str())
    }

    fn get_shape(&self, id: ShapeId) -> &Shape {
        self.shapes.get(id as usize).expect("Shape not found")
    }

    fn get_shape_mut(&mut self, id: ShapeId) -> &mut Shape {
        self.shapes.get_mut(id as usize).expect("Shape not found")
    }

    fn get_local(&self, id: LocalId) -> &Value {
        self.locals.get(id as usize).expect("local not found")
    }

    fn reserve_local(&mut self, value:Value) {
        self.locals.push(value);
    }

    fn clear_locals(&mut self) {
        self.locals.clear();
    }

    fn is_shape(&self, object_id: ObjectId, shape_id: ShapeId) -> bool {
        let object = self.get_object(object_id);
        let shape = self.get_shape(shape_id);

        shape.azimuths.iter().map(|id| self.get_azimuth(*id)).all(|slot| {
            if slot.is_static {
                let static_object = self.get_object(shape.static_object_id);
                object.has_azimuth(slot.id) || static_object.has_azimuth(slot.id)
            } else {
                object.has_azimuth(slot.id)
            }
        })
    }

    fn get_slot_value(&self, object_id: ObjectId, slot: AzimuthId) -> Option<&Value> {
        let object = self.get_object(object_id);
        if let Some(state_id) = object.get_slot_state_id(slot.clone()) {
            if let Some(state) = object.get_slot_state(state_id) {
                return Some(&state.storage);
            }
        }

        // Static
        let azimuth = self.get_azimuth(slot);
        if azimuth.is_static {
            let static_id = self.get_shape(azimuth.shape_id).static_object_id.clone();
            if static_id == object_id { return None; }
            
            return self.get_slot_value(static_id, slot);
        }
        None
    }

    fn get_slot_value_mut(&mut self, object_id: ObjectId, slot: AzimuthId) -> Option<&mut Value> {
        let object = self.get_object_mut(object_id);
        if let Some(state_id) = object.get_slot_state_id(slot.clone()) {
            if ((state_id - 1) as usize) < object.slot_states.len() {
                return Some(&mut object.slot_states[(state_id - 1) as usize].storage);
            }
        }
        None
    }

    fn get_slot_value_array_element(&self, object_id: ObjectId, slot: AzimuthId, index: usize) -> Option<&Value> {
        if let Some(Value::Array(arr, _)) = self.get_slot_value(object_id, slot) {
            return arr.get(index).map(|boxed| boxed.as_ref());
        }
        None
    }

    fn set_slot_value(&mut self, object_id: ObjectId, slot: AzimuthId, value: Value) {
        let object = self.get_object_mut(object_id);
        object.set_value(slot, value.clone());
        println!("Set obj{:?}.{:?} to {:?}", object.name, slot, value);
    }

    fn set_slot_value_primitive(&mut self, object_id: ObjectId, slot: AzimuthId, value: PrimitiveValue) {
        self.set_slot_value(object_id, slot, Value::Single(value));
    }

    fn set_slot_value_array(&mut self, object_id: ObjectId, slot: AzimuthId, values: Vec<Value>, kind: ValueKind) {
        self.set_slot_value(object_id, slot, Value::Array(values.into_iter().map(Box::new).collect(), kind));
    }
    fn set_slot_value_array_element(&mut self, object_id: ObjectId, slot: AzimuthId, index: usize, value: Value) {
        if let Some(Value::Array(arr, _)) = self.get_slot_value_mut(object_id, slot) {
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
                println!("{}", &state.storage);

                let mapped_slots: Vec<String> = object.slot_mapping.iter()
                    .filter(|(_, id)| *id == state_id)
                    .map(|(slot, _)| self.get_azimuth(slot.azimuth).name.clone())
                    .collect();
                println!("  Mapped slots: {}", mapped_slots.join(", "));

            }   
        }
        println!();
    }

}

//fn print_hello(params: FunctionParams) -> Value {
//    println!("Hello from native function! Caller: {}", params.caller);
//    Value::Single(PrimitiveValue::None)
//}

fn main() {

    let source = match fs::read_to_string("/workspaces/SlotBox/src/main.az") {
        Ok(source) => source,
        Err(_) => panic!("Could not find main.az"),
    };

    let tokens = tokenizer::tokenize(&source);

    let ast = match parser::parse(tokens) {
        Ok(ast) => ast,
        Err(error) => panic!("Could not parse main.az:\n{}", error),
    };

    let resolved_ast = match analyzer::analyze(ast){
        Ok(resolved_ast) => resolved_ast,
        Err(error) => panic!("Could not compile main.az:\n{}", error),
    };

    let mut runtime = Runtime::new();
    let result = match executor::execute(&mut runtime, resolved_ast){
        Ok(result) => result,
        Err(error) => panic!("Runtime error in main.az:\n{}", error),
    };
}
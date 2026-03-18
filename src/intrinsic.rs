use std::{env, io};
use rand::Rng;

use crate::{Number, Runtime, Value, ValueKind, executor::RuntimeError, lexer::Span, parser::{Expression, ParseError, ShapeExpression, Statement}};

pub type IntrinsicOp = fn(Span, Vec<Value>, &mut Runtime) -> Result<Value, RuntimeError>;

fn array_append(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let array_member = &args[0];
    let add = &args[1];

    match array_member {
        Value::Pointer(obj, az, _) => {
            runtime.push_array_element(*obj, *az, add.clone());
            Ok(true.into())
        }
        other => Err(RuntimeError::TypeMismatch{span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn array_insert(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let array_member = &args[0];
    let add = &args[1];
    let index = &args[2];

    match (array_member, index) {
        (Value::Pointer(obj, az, _), Value::Number(num)) => {
            runtime.insert_array_element(*obj, *az, num.to_i32() as usize, add.clone());
            Ok(true.into())
        }
        (other, index) => Err(RuntimeError::TypeMismatch{span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn array_remove(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let array_member = &args[0];
    let index = &args[1];

    match (array_member, index) {
        (Value::Pointer(obj, az, _), Value::Number(num)) => {
            runtime.remove_array_element(*obj, *az, num.to_i32() as usize);
            Ok(true.into())
        }
        (other, index) => Err(RuntimeError::TypeMismatch{span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn math_sqrt(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let operand = &args[0];

    match operand {
        //Value::Number(Number::Int32(val)) => sqrt()

        other => Err(RuntimeError::TypeMismatch{span, found: other.clone(), expected: ValueKind::Number }),
    }
}

fn io_readline(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let mut result = String::new();
    match io::stdin().read_line(&mut result) {
        Err(_) => Ok(Value::None),
        _ => Ok(result.trim().to_string().into()),
    }
}

fn io_args(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let cmd_args: Vec<String> = env::args().collect();
    let mut result = Vec::new();
    for arg in cmd_args {
        result.push(arg.into())
    }
    Ok(Value::Array(result, ValueKind::Array(Box::new(ValueKind::String))))
}

fn random_int(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let mut rng = rand::thread_rng();
    let random: i32 = rng.r#gen();
    Ok(random.into())
}

fn random_range(span: Span, args: Vec<Value>, runtime: &mut Runtime) -> Result<Value, RuntimeError> {
    let mut rng = rand::thread_rng();

    let from = &args[0];
    let to = &args[1];

    match (to, from) {
        (Value::Number(to), Value::Number(from)) => {
            let random: i32 = rng.gen_range(from.to_i32()..=to.to_i32());
            Ok(random.into())
        }
        _ => Err(RuntimeError::TypeMismatch{span, found: to.clone(), expected: ValueKind::Number })
    }
}

pub fn lookup(span: Span, name: String) -> Result<IntrinsicOp, ParseError> {
    match name.as_str() {
        "Array::Append" => Ok(array_append),
        "Array::Add" => Ok(array_insert),
        "Array::Remove" => Ok(array_remove),
        "Math::Sqrt" => Ok(math_sqrt),
        "IO::Args" => Ok(io_args),
        "IO::ReadLine" => Ok(io_readline),
        "Random::Int" => Ok(random_int),
        "Random::Range" => Ok(random_range),

        other => Err(ParseError::Error{span, message:format!("No intrinsic operation defined for {}", other)})
    }
}
use std::{env, io, string};
use rand::Rng;

use crate::{Number, Runtime, Value, ValueKind, analyzer::AzimuthInfo, executor::RuntimeError, lexer::Span, parser::{Expression, ParseError, ShapeExpression, Statement}};

pub struct IntrinsicParameters<'a> {
    pub span: Span,
    pub args: Vec<Value>,
    pub runtime: &'a mut Runtime,
    pub azimuth: Option<AzimuthInfo>,
}

pub type IntrinsicOp = fn(IntrinsicParameters) -> Result<Value, RuntimeError>;

fn array_append(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let array_member = &input.args[0];
    let add = &input.args[1];

    match array_member {
        Value::Pointer(obj, az, _) => {
            input.runtime.push_array_element(*obj, *az, add.clone());
            Ok(true.into())
        }
        other => Err(RuntimeError::TypeMismatch{span:input.span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn array_insert(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let array_member = &input.args[0];
    let add = &input.args[1];
    let index = &input.args[2];

    match (array_member, index) {
        (Value::Pointer(obj, az, _), Value::Number(num)) => {
            input.runtime.insert_array_element(*obj, *az, num.to_i32() as usize, add.clone());
            Ok(true.into())
        }
        (other, index) => Err(RuntimeError::TypeMismatch{span:input.span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn array_remove(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let array_member = &input.args[0];
    let index = &input.args[1];

    match (array_member, index) {
        (Value::Pointer(obj, az, _), Value::Number(num)) => {
            input.runtime.remove_array_element(*obj, *az, num.to_i32() as usize);
            Ok(true.into())
        }
        (other, index) => Err(RuntimeError::TypeMismatch{span:input.span, found: other.clone(), expected: ValueKind::Array(Box::new(ValueKind::None)) }),
    }
}

fn math_sqrt(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let operand = &input.args[0];

    match operand {
        //Value::Number(Number::Int32(val)) => sqrt()

        other => Err(RuntimeError::TypeMismatch{span:input.span, found: other.clone(), expected: ValueKind::Number }),
    }
}

fn io_readline(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let mut result = String::new();
    match io::stdin().read_line(&mut result) {
        Err(_) => Ok(Value::None),
        _ => Ok(result.trim().to_string().into()),
    }
}

fn io_args(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let cmd_args: Vec<String> = env::args().collect();
    let mut result = Vec::new();
    for arg in cmd_args {
        result.push(arg.into())
    }
    Ok(Value::Array(result, ValueKind::Array(Box::new(ValueKind::String))))
}

fn random_int(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let mut rng = rand::thread_rng();
    let random: i32 = rng.r#gen();
    Ok(random.into())
}

fn random_range(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let mut rng = rand::thread_rng();

    let from = &input.args[0];
    let to = &input.args[1];

    match (to, from) {
        (Value::Number(to), Value::Number(from)) => {
            let random: i32 = rng.gen_range(from.to_i32()..=to.to_i32());
            Ok(random.into())
        }
        _ => Err(RuntimeError::TypeMismatch{span:input.span, found: to.clone(), expected: ValueKind::Number })
    }
}

fn string_upper(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let val = &input.args[0];
    match val {
        Value::String(string) => Ok(Value::String(string.to_uppercase())),
        other => Err(RuntimeError::TypeMismatch{span:input.span, found:other.clone(), expected: ValueKind::String })
    }
}

fn string_lower(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let val = &input.args[0];
    match val {
        Value::String(string) => Ok(Value::String(string.to_lowercase())),
        other => Err(RuntimeError::TypeMismatch{span:input.span, found:other.clone(), expected: ValueKind::String })
    }
}

fn string_trim(input: IntrinsicParameters) -> Result<Value, RuntimeError> {
    let val = &input.args[0];
    match val {
        Value::String(string) => Ok(Value::String(string.trim().to_string())),
        other => Err(RuntimeError::TypeMismatch{span:input.span, found:other.clone(), expected: ValueKind::String })
    }
}

pub fn lookup(span: Span, name: String) -> Result<IntrinsicOp, ParseError> {
    match name.as_str() {
        "Array::Append" => Ok(array_append),
        "Array::Add" => Ok(array_insert),
        "Array::Remove" => Ok(array_remove),
        "Sqrt" => Ok(math_sqrt),
        "Args" => Ok(io_args),
        "ReadLine" => Ok(io_readline),
        "Int" => Ok(random_int),
        "Range" => Ok(random_range),
        "String::Upper" => Ok(string_upper),
        "String::Lower" => Ok(string_lower),
        "String::Trim" => Ok(string_trim),

        other => Err(ParseError::Error{span, message:format!("No intrinsic operation defined for {}", other)})
    }
}
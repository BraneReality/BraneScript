use std::sync::LazyLock;

use crate::types::*;
use macros::CompilerTyApi;

#[derive(CompilerTyApi)]
pub enum Bool {
    True,
    False,
}

static NUMBER: LazyLock<Ty> = LazyLock::new(|| {
    Ty::Number(Number {
        value: NumberState::Range(
            NumberRange {
                begin: RangeBound {
                    inclusive: true,
                    value: NumberValue::Float(f64::MIN),
                },
                end: RangeBound {
                    inclusive: true,
                    value: NumberValue::Float(f64::MAX),
                },
            }
            .into(),
        ),
        bit_width: NumberBitWidth::Unresolved,
        storage_type: StorageType::Unresolved,
    })
});

// TODO: Add intrinsics for shared types

#[derive(CompilerTyApi)]
pub struct PipelineId {
    pub value: String,
}

pub fn define() -> Object {
    let add = ObjectMember::new(
        "add",
        Ty::intrinsic_fn(
            [("a", NUMBER.clone()), ("a", NUMBER.clone())],
            |args: Vec<Ty>, _| -> Result<Ty, Vec<Error>> {
                let a = &args[0];
                let b = &args[1];
                if let (Ty::Number(a), Ty::Number(b)) = (&a, &b) {
                    let value = match (&a.value, &b.value) {
                        (NumberState::Const(a), NumberState::Const(b)) => {
                            NumberState::Const(match (a, b) {
                                (NumberValue::Int(a), NumberValue::Int(b)) => {
                                    NumberValue::Int(a + b)
                                }
                                (NumberValue::Int(i), NumberValue::Float(f))
                                | (NumberValue::Float(f), NumberValue::Int(i)) => {
                                    NumberValue::Float(f + (*i as f64))
                                }
                                (NumberValue::Float(a), NumberValue::Float(b)) => {
                                    NumberValue::Float(a + b)
                                }
                            })
                        }
                        (NumberState::Const(c), NumberState::Range(r))
                        | (NumberState::Range(r), NumberState::Const(c)) => {
                            todo!()
                        }
                        (NumberState::Range(a), NumberState::Range(b)) => todo!(),
                    };

                    Ok(Ty::Number(Number {
                        value,
                        // Temp, we need to actually handle these
                        bit_width: a.bit_width,
                        storage_type: a.storage_type,
                    }))
                } else {
                    Err(vec![Error::new(format!(
                        "Found non-number argument {} or {}",
                        a, b
                    ))])
                }
            },
        ),
    );

    Object::new(
        Some("std".into()),
        [
            ObjectMember::new("True", Bool::True.as_ty()),
            ObjectMember::new("False", Bool::False.as_ty()),
            ObjectMember::new("Bool", Bool::ty()),
            ObjectMember::new("Number", NUMBER.clone()),
            add,
        ],
    )
}

use crate::types::*;
use macros::CompilerValueApi;

#[derive(CompilerValueApi)]
pub enum Bool {
    True,
    False,
}

pub mod value {
    use std::collections::HashSet;

    use crate::{types::*, Interpreter};
    use macros::CompilerValueApi;

    #[derive(CompilerValueApi, Debug)]
    pub struct EnumVariantSpec {
        pub label: String,
        // Lazy evaluated Spec
        pub spec: Option<CompilerValue>,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct ArraySpec {
        pub spec: Option<Box<Spec>>,
        pub len: Option<usize>,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct StructMemberSpec {
        label: String,
        // Lazy evaluated Spec
        spec: Box<CompilerValue>,
    }

    #[derive(CompilerValueApi, Debug)]
    pub enum StructSpecStrictness {
        Exact,
        Unordered,
        Interface,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct StructSpec {
        pub members: Vec<StructMemberSpec>,
        pub strictness: StructSpecStrictness,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct ParamSpec {
        pub constraints: Vec<CompilerValue>,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct FunctionParam {
        pub label: String,
        pub constraints: Vec<CompilerValue>,
    }

    #[derive(CompilerValueApi, Debug)]
    pub struct FunctionSpec {
        pub params: Vec<ParamSpec>,
        pub returns: Vec<CompilerValue>,
        pub strictness: StructSpecStrictness,
    }

    #[derive(CompilerValueApi, Debug)]
    pub enum Spec {
        Number,
        String,
        Error,
        EnumVariant(EnumVariantSpec),
        Array(ArraySpec),
        Struct(StructSpec),
        Function(FunctionSpec),
        Options(Vec<Spec>),
        Any,
    }

    pub fn resolve_spec(spec: CompilerValue, vm: Option<&Interpreter>) -> Result<Spec, Error> {
        match &spec.kind {
            CompilerValueKind::Object(_) => Ok(Spec::from_value(spec)?),
            CompilerValueKind::Function(func) => Spec::from_value(if let Some(vm) = vm {
                vm.call(func, vec![])
            } else {
                Interpreter::default().call(func, vec![])
            }),
            _ => {
                return Err(Error {
                    stack_trace: Vec::new(),
                    message: format!("Check spec was passed non-spec value {}", spec),
                })
            }
        }
    }

    pub fn check_value_spec(
        value: &CompilerValue,
        spec: CompilerValue,
        vm: Option<&Interpreter>,
    ) -> Result<(), Error> {
        check_spec(value, &(resolve_spec(spec, vm)?), vm)
    }

    pub fn check_spec(
        value: &CompilerValue,
        spec: &Spec,
        vm: Option<&Interpreter>,
    ) -> Result<(), Error> {
        match (&value.kind, spec) {
            (_, Spec::Any)
            | (CompilerValueKind::Number(_), Spec::Number)
            | (CompilerValueKind::String(_), Spec::String)
            | (CompilerValueKind::Error(_), Spec::Error) => Ok(()),
            (CompilerValueKind::EnumVariant(enum_variant), Spec::EnumVariant(enum_spec)) => {
                if enum_variant.label != enum_spec.label {
                    return Err(Error {
                        stack_trace: Vec::new(),
                        message: format!(
                            "Specified enum variant {} but got {}",
                            enum_variant.label, enum_spec.label
                        ),
                    });
                }

                match &enum_spec.spec.as_ref() {
                    Some(data_spec) => match &enum_variant.value {
                        Some(enum_value) => check_value_spec(enum_value, (*data_spec).clone(), vm),
                        None => Err(Error {
                            stack_trace: Vec::new(),
                            message: "Specified enum variant required data but it was empty".into(),
                        }),
                    },
                    None => {
                        if enum_variant.value.is_some() {
                            Err(Error {
                                stack_trace: Vec::new(),
                                message: "Specified enum variant is empty but the checked value contains data".into(),
                            })
                        } else {
                            Ok(())
                        }
                    }
                }
            }
            (CompilerValueKind::Array(array), Spec::Array(array_spec)) => {
                if let Some(len) = array_spec.len {
                    if len != array.values.len() {
                        return Err(Error {
                            stack_trace: Vec::new(),
                            message: format!(
                                "Specified array of length {} but got array of lenth {}",
                                len,
                                array.values.len()
                            ),
                        });
                    }
                }

                if let Some(spec) = array_spec.spec.as_ref() {
                    for value in array.values.iter() {
                        check_spec(value, &spec, vm)?;
                    }
                }

                Ok(())
            }
            (CompilerValueKind::Object(object), Spec::Struct(object_spec)) => {
                match object_spec.strictness {
                    StructSpecStrictness::Exact => {
                        for (i, m_spec) in object_spec.members.iter().enumerate() {
                            let member = object.members().get(i).ok_or_else(|| Error {
                                stack_trace: Vec::new(),
                                message: format!("Object missing \"{}\" member", m_spec.label),
                            })?;
                            if member.label != m_spec.label {
                                return Err(Error {
                                    stack_trace: Vec::new(),
                                    message: format!("Expected object to have {} member at index {}, but found {}", m_spec.label, i, member.label),
                                });
                            }
                            check_value_spec(&member.value, (*m_spec.spec).clone(), vm)?;
                        }
                        if object_spec.members.len() < object.members().len() {
                            Err(Error {
                                stack_trace: Vec::new(),
                                message: format!(
                                    "Object has extra members: {}",
                                    object.members()
                                        [object_spec.members.len()..object.members().len()]
                                        .iter()
                                        .map(|m| m.label.clone())
                                        .reduce(|a, b| format!("{}, {}", a, b))
                                        .unwrap_or_default()
                                ),
                            })
                        } else {
                            Ok(())
                        }
                    }
                    StructSpecStrictness::Unordered => {
                        let mut extra_members: HashSet<_> =
                            object.members().iter().map(|m| &m.label).collect();
                        for m_spec in object_spec.members.iter() {
                            let member = object.get(&m_spec.label).ok_or_else(|| Error {
                                stack_trace: Vec::new(),
                                message: format!("Object missing \"{}\" member", m_spec.label),
                            })?;
                            check_value_spec(&member.value, (*m_spec.spec).clone(), vm)?;
                            extra_members.remove(&member.label);
                        }
                        if !extra_members.is_empty() {
                            Err(Error {
                                stack_trace: Vec::new(),
                                message: format!(
                                    "Object has extra members: {}",
                                    extra_members
                                        .iter()
                                        .map(|s| (*s).clone())
                                        .reduce(|a, b| format!("{}, {}", a, b))
                                        .unwrap_or_default()
                                ),
                            })
                        } else {
                            Ok(())
                        }
                    }
                    StructSpecStrictness::Interface => {
                        for m_spec in object_spec.members.iter() {
                            let member = object.get(&m_spec.label).ok_or_else(|| Error {
                                stack_trace: Vec::new(),
                                message: format!("Object missing \"{}\" member", m_spec.label),
                            })?;
                            check_value_spec(&member.value, (*m_spec.spec).clone(), vm)?;
                        }
                        Ok(())
                    }
                }
            }
            (CompilerValueKind::Function(function), Spec::Function(function_spec)) => {
                if function.params.values.len() != function_spec.params.len() {
                    return Err(Error {
                        stack_trace: Vec::new(),
                        message: format!(
                            "Specified function with {} params, but got one with {}",
                            function_spec.params.len(),
                            function.params.values.len()
                        ),
                    });
                }

                for (param, param_spec) in function
                    .params
                    .values
                    .iter()
                    .zip(function_spec.params.iter())
                {
                    let param = FunctionParam::from_value(param.clone())?;
                    for constraint in param.constraints.iter() {
                        let mut valid_constraint = false;
                        for c_spec in param_spec.constraints.iter() {
                            if constraint == c_spec {
                                valid_constraint = true;
                                break;
                            }
                        }

                        if !valid_constraint {
                            return Err(Error {
                                stack_trace: Vec::new(),
                                message: format!(
                                    "Function has uncompatable constraint {}",
                                    constraint,
                                ),
                            });
                        }
                    }
                }

                for constraint in function_spec.returns.iter() {
                    let mut valid_constraint = false;
                    for c_spec in function.returns.values.iter() {
                        if constraint == c_spec {
                            valid_constraint = true;
                            break;
                        }
                    }

                    if !valid_constraint {
                        return Err(Error {
                            stack_trace: Vec::new(),
                            message: format!(
                                "Function return value does not have constraint {}",
                                constraint,
                            ),
                        });
                    }
                }

                Ok(())
            }
            _ => Err(Error {
                stack_trace: Vec::new(),
                message: format!("Value type {} does not match spec {:?}", value, spec),
            }),
        }
    }

    pub fn define() -> CompilerValue {
        let must_be = CompilerValue::intrinsic_fn([("spec", vec![])], |mut args, _| {
            let spec = Spec::from_value(args.pop().expect("must_be requires one arg!"));
            match spec {
                Ok(spec) => {
                    CompilerValue::intrinsic_fn([("value", vec![])], move |mut args, vm| {
                        let arg = args.pop().expect("must_be requires one arg!");
                        println!("value {} must be {:?}", arg, spec);
                        match check_spec(&arg, &spec, Some(vm)) {
                            Ok(_) => arg,
                            Err(err) => CompilerValue {
                                kind: CompilerValueKind::Error(err),
                                share_info: None,
                            },
                        }
                    })
                }
                Err(err) => CompilerValue {
                    kind: CompilerValueKind::Error(err),
                    share_info: None,
                },
            }
        });

        CompilerValue::object([
            ObjectMember::new("Spec", Spec::spec()),
            ObjectMember::new("must_be", must_be),
        ])
    }
}

pub fn define() -> Object {
    let add = ObjectMember::new(
        "add",
        CompilerValue::intrinsic_fn([("a", vec![]), ("a", vec![])], |args, _| {
            let a = &args[0];
            let b = &args[1];
            if let (CompilerValueKind::Number(a), CompilerValueKind::Number(b)) = (&a.kind, &b.kind)
            {
                CompilerValue::number(a.value + b.value)
            } else {
                unreachable!("Should be guarded by constraints");
            }
        }),
    );

    Object::new([
        ObjectMember::new("True", CompilerValue::variant("True", None)),
        ObjectMember::new("False", CompilerValue::variant("False", None)),
        ObjectMember::new("Bool", Bool::spec()),
        ObjectMember::new("Number", value::Spec::Number.to_value()),
        ObjectMember::new("Value", value::define()),
        add,
    ])
}

use brane_core::ir::IRModule;
use std::{collections::HashMap, sync::Mutex};

pub mod stdlib;
pub mod types;
use types::{Error, Expression, Function, Ty, Value};

/// Interprets a script to create an IR module
pub struct Interpreter {
    pub intrinsics: HashMap<String, Ty>,
    pub module: IRModule,
    pub current_function: Vec<IRFunction>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            intrinsics: Default::default(),
            module: Mutex::new(IRModule {
                id: "DEFAULT".into(),
                structs: Vec::new(),
                pipelines: Vec::new(),
            }),
            current_function,
        }
    }

    /// Runs a script in object form
    pub fn call(
        &mut self,
        function: &Function,
        args: impl IntoIterator<Item = Value, IntoIter = impl ExactSizeIterator<Item = Value>>,
    ) -> Result<Value, Vec<Error>> {
        let args = args.into_iter();

        if args.len() != function.params.len() {
            return Err(vec![Error::new(format!(
                "function call was expecting {} args, but was passed {} args",
                function.params.len(),
                args.len(),
            ))]);
        }

        // Convert args to labeled values while checking constraints
        let (label_args, errors) = args.zip(function.params.iter()).fold(
            (Vec::new(), Vec::new()),
            |(mut label_args, mut errors), (arg, param)| {
                match param.ty.contains(&arg.ty) {
                    Ok(_) => label_args.push((param.label.clone(), arg)),
                    Err(mut errs) => errors.append(&mut errs),
                }
                (label_args, errors)
            },
        );

        if !errors.is_empty() {
            return Err(errors);
        }

        match &function.defintion {
            types::FunctionDefinition::Intrinsic(closure) => closure(
                label_args.into_iter().map(|(_label, arg)| arg).collect(),
                self,
            ),
            types::FunctionDefinition::Closure { operations, value } => {
                let mut labels: HashMap<String, Value> = label_args.into_iter().collect();
                for op in operations {
                    match op {
                        types::LabelOperation::Label(label, ty, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels)?;

                            if let Some(ty) = ty {
                                let value = self.evaluate(ty, &mut labels)?;
                                value.ty.contains(&compiler_value.ty)?;
                            }
                            labels.insert(label.clone(), compiler_value);
                        }
                        types::LabelOperation::Destructure(items, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels)?;
                            if let Ty::Object(object) = compiler_value.ty {
                                let mut members: HashMap<String, Value> = object.unpack().collect();
                                for (member, label) in items.iter() {
                                    if let Some(member_value) = members.remove(member) {
                                        labels.insert(
                                            label.clone().unwrap_or(member.clone()),
                                            member_value,
                                        );
                                    } else {
                                        return Err(vec![Error::new(format!(
                                            "Object does not have member {}!",
                                            member
                                        ))]);
                                    }
                                }
                            } else {
                                return Err(vec![Error::new(format!(
                                    "Value {} was not an object, and cannot be destructured",
                                    compiler_value
                                ))]);
                            }
                        }
                    }
                }

                self.evaluate(value, &mut labels)
            }
        }
    }

    /// Evaluate an expression to a value
    pub fn evaluate(
        &mut self,
        expr: &Expression,
        labels: &mut HashMap<String, Value>,
    ) -> Result<Value, Vec<Error>> {
        use types::*;
        match &expr {
            Expression::Ty(ty) => Ok(Value {
                ty: ty.clone(),
                value: None,
            }),
            Expression::Label(label) => {
                println!("Evaluating label \"{}\"", label);
                if let Some(value) = labels.remove(label) {
                    Ok(value)
                } else if let Some(intrinsic) = self.intrinsics.get(label) {
                    Ok(Value {
                        ty: intrinsic.clone(),
                        value: None,
                    })
                } else {
                    Err(vec![Error::new(format!(
                        "Label {} does not exist in this context",
                        label
                    ))])
                }
            }
            Expression::Call(function_expr, args) => {
                println!("Evaluating call");
                let mut errors = Vec::new();
                let function_value = self.evaluate(&function_expr, labels);

                let mut arg_values = Vec::new();
                for arg in args {
                    match self.evaluate(arg, labels) {
                        Ok(arg) => arg_values.push(arg),
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }
                //TODO: Handle enforcement of shared function arguments returning and re-labeling

                if errors.is_empty() {
                    match function_value {
                        Ok(function) => {
                            if let Ty::Function(function) = function.ty {
                                self.call(&function, arg_values)
                            } else {
                                Err(vec![Error::new(format!(
                                    "Value {} was not a function, and cannot be called",
                                    function
                                ))])
                            }
                        }
                        Err(errs) => Err(errs),
                    }
                } else {
                    Err(errors)
                }
            }
            Expression::Error(errors) => Err(errors.clone()),
            Expression::Consumed => Err(vec![Error::new("Tried to use consumed value")]),
            Expression::Structure(ty, members) => {
                let mut member_values = Vec::new();
                let mut errors = Vec::new();
                for (label, speculative, expr) in members {
                    match self.evaluate(expr, labels) {
                        Ok(value) => member_values.push(ObjectMember {
                            label: label.clone(),
                            speculative: *speculative,
                            value,
                        }),
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }

                if errors.is_empty() {
                    let mut object = Object::new(None, member_values);

                    match ty.as_ref().map(|value| self.evaluate(value, labels)) {
                        Some(Ok(value)) => match &value.ty {
                            Ty::Object(obj_ty) => {
                                object.label = obj_ty.label.clone();
                                let ret = Value {
                                    ty: Ty::Object(object),
                                    value: None,
                                };
                                match ret.ty.contains(&value.ty) {
                                    Ok(()) => Ok(ret),
                                    Err(errors) => Err(errors),
                                }
                            }
                            _ => Err(vec![Error::new(format!(
                                "Expected an object type value, but found: {}",
                                value
                            ))]),
                        },
                        Some(Err(errors)) => Err(errors),
                        None => Ok(Value {
                            ty: Ty::Object(object),
                            value: None,
                        }),
                    }
                } else {
                    Err(errors)
                }
            }
            Expression::EnumVariant(ty, variant, value) => {
                let variants = vec![match value {
                    Some(value) => EnumVariant {
                        label: variant.clone(),
                        value: Some(self.evaluate(value, labels)?),
                    },
                    None => EnumVariant {
                        label: variant.clone(),
                        value: None,
                    },
                }];
                let mut value_enum = Enum {
                    label: None,
                    variants,
                };
                match ty {
                    Some(ty) => {
                        if let Value {
                            ty: Ty::Enum(ty_enum),
                            value,
                        } = self.evaluate(&ty, labels)?
                        {
                            value_enum.label = ty_enum.label.clone();
                            let res = Ty::Enum(value_enum);
                            Ty::Enum(ty_enum).contains(&res)?;

                            Ok(Value {
                                ty: Ty::Enum(ty_enum),
                                value,
                            })
                        } else {
                            Err(vec![Error::new(format!(
                                "Expected an enum type value, but found: {}",
                                ty
                            ))])
                        }
                    }
                    None => Ok(Value {
                        ty: Ty::Enum(value_enum),
                        value: None,
                    }),
                }
            }
            Expression::Match(condition, branches) => {
                let condition = self.evaluate(&condition, labels)?;
                let (branches, errors) = branches.iter().fold(
                    (Vec::new(), Vec::new()),
                    |(mut branches, mut errors), func| {
                        match self.evaluate(&func, labels) {
                            Ok(func) => {
                                if let Value {
                                    ty: Ty::Function(func),
                                    value: _,
                                } = func
                                {
                                    if func.params.len() == 1 {
                                        branches.push(func);
                                    } else {
                                        errors.push(Error::new(
                                            "Can only match on single parameter functions",
                                        ))
                                    }
                                } else {
                                    errors.push(Error::new("Expected Function Value"))
                                }
                            }
                            Err(mut errs) => {
                                errors.append(&mut errs);
                            }
                        }
                        (branches, errors)
                    },
                );

                if errors.is_empty() {
                    for branch in branches {
                        if let Err(_) = branch.params[0].ty.contains(&condition.ty) {
                            continue;
                        }

                        return self.call(&branch, [condition]);
                    }
                    Err(vec![Error::new(format!(
                        "No branch of match statement accepts {}",
                        condition
                    ))])
                } else {
                    Err(errors)
                }
            }
            Expression::ConstructFunction(params, returns, operations, value) => {
                let (params, mut errors) = params.iter().fold(
                    (Vec::new(), Vec::new()),
                    |(mut params, mut errors), (label, type_value)| {
                        match self.evaluate(type_value, labels) {
                            Ok(ty) => params.push(FunctionParam {
                                label: label.clone(),
                                ty: ty.ty,
                            }),
                            Err(mut errs) => errors.append(&mut errs),
                        }
                        (params, errors)
                    },
                );
                let returns = self.evaluate(&returns, labels);
                let returns = match returns {
                    Ok(r) => Box::new(r.ty),
                    Err(mut errs) => {
                        errors.append(&mut errs);
                        return Err(errors);
                    }
                };

                if !errors.is_empty() {
                    return Err(errors);
                }

                //TODO validate function body

                Ok(Value {
                    ty: Ty::Function(Function {
                        params,
                        returns,
                        defintion: FunctionDefinition::Closure {
                            operations: operations.clone(),
                            value: value.clone(),
                        },
                    }),
                    value: None,
                })
            }
            Expression::ConstructPipeline(params, ret_type, segments) => {
                let params = params
                    .iter()
                    .map(|(label, param)| {
                        Result::<_, Vec<Error>>::Ok((
                            label.clone(),
                            self.evaluate(param, labels)?.ty,
                        ))
                    })
                    .collect::<Result<Vec<(String, Ty)>, Vec<Error>>>()?;

                let ret = self.evaluate(&ret_type, labels)?;

                let (segments, _) = segments.iter().fold(
                    Ok((params, Vec::new())),
                    |args, segment| {
                        let (last_out, mut segments) = match args {
                            Err(err) => return Err(err),
                            Ok(v) => v,
                        };

                        let segment = self.evaluate(segment, labels)?;

                        if let Ty::Function(func) = &segment.ty {
                            if func.params.len() != last_out.len() {
                                return Err(vec![Error::new(format!(
                                    "Expected pipeline segment function with {} params, but found one with {}",
                                    last_out.len(),
                                    func.params.len()
                                ))]);
                            }

                            let errors = func.params.iter().zip(last_out.iter()).enumerate().fold(Vec::new(), |mut errors, (index, (param, (_, input_ty)))|{
                                if let Err(mut e) = param.ty.contains(&input_ty) {
                                    errors.append(&mut e);
                                }
                                errors
                            });

                            if !errors.is_empty() {
                                return Err(errors);
                            }

                            segments.push(segment);
                            Ok((
                                func.params
                                    .iter()
                                    .map(|p| (p.label.clone(), p.ty.clone()))
                                    .collect::<Vec<(String, Ty)>>(),
                                segments,
                            ))
                        } else {
                            Err(vec![Error::new(format!(
                                "Expected pipeline segment to be function, but found: {}",
                                segment
                            ))])
                        }
                    },
                )?;

                Ok(Value {
                    ty: todo!(),
                    value: todo!(),
                })
            }
        }
    }
}

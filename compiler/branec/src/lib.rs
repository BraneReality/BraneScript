use std::collections::HashMap;

pub mod stdlib;
pub mod types;
use types::{Error, Expression, Function, Value};

/// Interprets a script to create an IR module
#[derive(Default)]
pub struct Interpreter {
    pub intrinsics: HashMap<String, Value>,
}

impl Interpreter {
    /// Runs a script in object form
    pub fn call(
        &self,
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
                match param.ty.contains(&arg) {
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
                                let ty = self.evaluate(ty, &mut labels)?;
                                ty.contains(&compiler_value)?;
                            }
                            labels.insert(label.clone(), compiler_value);
                        }
                        types::LabelOperation::Destructure(items, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels)?;
                            if let Value::Object(object) = compiler_value {
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
    /// this might be called a "beta_reduction"
    pub fn evaluate(
        &self,
        expr: &Expression,
        labels: &mut HashMap<String, Value>,
    ) -> Result<Value, Vec<Error>> {
        use types::*;
        match &expr {
            Expression::Value(value) => Ok(value.clone()),
            Expression::Label(label) => {
                println!("Evaluating label \"{}\"", label);
                if let Some(value) = labels.remove(label) {
                    Ok(value)
                } else if let Some(intrinsic) = self.intrinsics.get(label) {
                    Ok(intrinsic.clone())
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

                if errors.is_empty() {
                    match function_value {
                        Ok(function) => {
                            if let Value::Function(function) = function {
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

                    match ty.as_ref().map(|ty| self.evaluate(ty, labels)) {
                        Some(Ok(ty)) => match &ty {
                            Value::Object(obj_ty) => {
                                object.label = obj_ty.label.clone();
                                let ret = Value::Object(object);
                                match ty.contains(&ret) {
                                    Ok(()) => Ok(ret),
                                    Err(errors) => Err(errors),
                                }
                            }
                            _ => Err(vec![Error::new(format!(
                                "Expected an object type value, but found: {}",
                                ty
                            ))]),
                        },
                        Some(Err(errors)) => Err(errors),
                        None => Ok(Value::Object(object)),
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
                        if let Value::Enum(ty_enum) = self.evaluate(&ty, labels)? {
                            value_enum.label = ty_enum.label.clone();
                            let res = Value::Enum(value_enum);
                            Value::Enum(ty_enum).contains(&res)?;
                            Ok(res)
                        } else {
                            Err(vec![Error::new(format!(
                                "Expected an enum type value, but found: {}",
                                ty
                            ))])
                        }
                    }
                    None => Ok(Value::Enum(value_enum)),
                }
            }
            Expression::Match(condition, branches) => {
                let condition = self.evaluate(&condition, labels)?;
                let (branches, errors) = branches.iter().fold(
                    (Vec::new(), Vec::new()),
                    |(mut branches, mut errors), (ty, func)| {
                        match (self.evaluate(&ty, labels), self.evaluate(&func, labels)) {
                            (Ok(ty), Ok(func)) => {
                                // TODO validate type is type, and function sig accepts type
                                if let Value::Function(func) = func {
                                    branches.push((ty, func));
                                } else {
                                    errors.push(Error::new("Expected Function Value"))
                                }
                            }
                            (Ok(_), Err(mut err)) | (Err(mut err), Ok(_)) => {
                                errors.append(&mut err)
                            }
                            (Err(mut e1), Err(mut e2)) => {
                                errors.append(&mut e1);
                                errors.append(&mut e2);
                            }
                        }
                        (branches, errors)
                    },
                );

                if errors.is_empty() {
                    for (cond, func) in branches {
                        if let Err(_) = cond.contains(&condition) {
                            continue;
                        }

                        return self.call(&func, [condition]);
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
                                ty,
                            }),
                            Err(mut errs) => errors.append(&mut errs),
                        }
                        (params, errors)
                    },
                );
                let returns = self.evaluate(&returns, labels);
                let returns = match returns {
                    Ok(r) => Box::new(r),
                    Err(mut errs) => {
                        errors.append(&mut errs);
                        return Err(errors);
                    }
                };

                if !errors.is_empty() {
                    return Err(errors);
                }

                //TODO validate function body

                Ok(Value::Function(Function {
                    params,
                    returns,
                    defintion: FunctionDefinition::Closure {
                        operations: operations.clone(),
                        value: value.clone(),
                    },
                }))
            }
        }
    }
}

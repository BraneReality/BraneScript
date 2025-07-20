use std::collections::HashMap;

pub mod types;
use types::{CompilerValue, CompilerValueKind, Error, Function};

macro_rules! compiler_error {
    ($value:expr, $($msg:tt)*) => {
        CompilerValue {
            kind: CompilerValueKind::Error(Error {
                stack_trace: vec![format!("evaluate {:?}", $value)],
                message: format!($($msg)*),
            }),
            share_info: None,
        }
    };
}

/// Interprets a script to create an IR module
pub struct Interpreter {
    pub intrinsics: HashMap<String, CompilerValue>,
}

impl Interpreter {
    /// Runs a script in object form
    pub fn call(
        &self,
        function: &Function,
        args: impl IntoIterator<
            Item = CompilerValue,
            IntoIter = impl ExactSizeIterator<Item = CompilerValue>,
        >,
    ) -> CompilerValue {
        let args = args.into_iter();
        if args.len() != function.params.values.len() {
            return compiler_error!(
                function,
                "function call was expecting {} args, but was passed {} args",
                function.params.values.len(),
                args.len()
            );
        }

        // Convert args to labeled values while checking constraints
        let mut label_args = Vec::new();
        for (arg, param) in args.zip(function.params.values.iter()) {
            let Some(param) = param.as_object() else {
                return compiler_error!(
                    function,
                    "function parameter was not an object, found: {:?}",
                    param
                );
            };

            let Some(label) = param.get("label").map(|s| s.value.as_string()).flatten() else {
                return compiler_error!(
                    function,
                    "function parameter did not have string member label: {:?}",
                    param
                );
            };

            let Some(constraints) = param
                .get("constraints")
                .map(|s| s.value.as_array())
                .flatten()
            else {
                return compiler_error!(
                    function,
                    "function parameter did not have array member constraints: {:?}",
                    param
                );
            };

            let arg = constraints.values.iter().fold(arg, |arg, constraint| {
                self.evaluate_constraint(constraint, arg)
            });

            if let CompilerValueKind::Error(_) = arg.kind {
                // TODO append stack trace
                return arg.clone();
            }
            label_args.push((label.clone(), arg));
        }

        match &function.defintion {
            types::FunctionDefinition::Intrinsic(closure) => {
                closure(label_args.into_iter().map(|(_label, arg)| arg).collect())
            }
            types::FunctionDefinition::Closure { operations, value } => {
                let mut labels = label_args.into_iter().collect();
                for op in operations {
                    match op {
                        types::LabelOperation::Label(label, constraints, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels);

                            let compiler_value = match constraints {
                                Some(constraint) => {
                                    let constraint = self.evaluate(constraint, &mut labels);
                                    self.evaluate_constraint(&constraint, compiler_value)
                                }
                                None => compiler_value,
                            };

                            labels.insert(label.clone(), compiler_value);
                        }
                        types::LabelOperation::Destructure(items, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels);
                            if let CompilerValue {
                                kind: CompilerValueKind::Object(object),
                                share_info: _,
                            } = compiler_value
                            {
                                for (member, label) in items.iter() {
                                    if let Some(member_value) = object.get(&member) {
                                        let member_value =
                                            self.evaluate(&member_value.value, &mut labels);
                                        labels.insert(
                                            label.clone().unwrap_or(member.clone()),
                                            member_value,
                                        );
                                    } else {
                                        return compiler_error!(
                                            function,
                                            "Object does not have member {}!",
                                            member
                                        );
                                    }
                                }
                            } else {
                                return compiler_error!(
                                    function,
                                    "Value {:?} was not an object, and cannot be destructured",
                                    compiler_value
                                );
                            }
                        }
                    }
                }

                self.evaluate(value, &mut labels)
            }
        }
    }

    /// Simplify value as much as possible by resolving labels and evaluating functions
    /// this might be called a "beta_reduction"
    pub fn evaluate(
        &self,
        value: &CompilerValue,
        labels: &mut HashMap<String, CompilerValue>,
    ) -> CompilerValue {
        use types::*;
        match &value.kind {
            CompilerValueKind::Number(_)
            | CompilerValueKind::String(_)
            | CompilerValueKind::Error(_) => value.clone(),
            CompilerValueKind::Label(label) => {
                println!("Evaluating label \"{}\"", label);
                if let Some(value) = labels.remove(label) {
                    value
                } else if let Some(intrinsic) = self.intrinsics.get(label) {
                    self.evaluate(intrinsic, labels)
                } else {
                    compiler_error!(value, "Label {} does not exist in this context", label)
                }
            }
            CompilerValueKind::Object(object) => {
                CompilerValue::object(object.members().iter().map(|m| ObjectMember {
                    label: m.label.clone(),
                    value: self.evaluate(&m.value, labels),
                }))
            }
            CompilerValueKind::Array(array) => CompilerValue {
                kind: CompilerValueKind::Array(Array {
                    values: array
                        .values
                        .iter()
                        .map(|v| self.evaluate(v, labels))
                        .collect(),
                }),
                share_info: None,
            },
            CompilerValueKind::EnumVariant(variant) => {
                println!("Evaluating enum variant {}", variant.label);
                CompilerValue::variant(
                    variant.label.clone(),
                    variant.value.as_ref().map(|v| self.evaluate(&v, labels)),
                )
            }
            CompilerValueKind::Function(function) => {
                println!(
                    "Evaluating function with {} params",
                    function.params.values.len()
                );

                CompilerValue {
                    kind: CompilerValueKind::Function(Function {
                        params: Array {
                            values: function
                                .params
                                .values
                                .iter()
                                .map(|value| self.evaluate(value, labels))
                                .collect(),
                        },
                        defintion: function.defintion.clone(),
                    }),
                    share_info: value.share_info.clone(),
                }
            }
            CompilerValueKind::Call(function_value, args) => {
                println!("Evaluating call");
                let function_value = self.evaluate(function_value, labels);
                if let CompilerValue {
                    kind: CompilerValueKind::Function(function),
                    share_info: _,
                } = function_value
                {
                    let args = args.iter().map(|arg| self.evaluate(arg, labels));
                    self.call(&function, args)
                } else {
                    compiler_error!(
                        value,
                        "Value {:?} was not a function, and cannot be called",
                        function_value
                    )
                }
            }
            CompilerValueKind::Match(enum_variant_value, branches) => {
                let enum_variant_value = self.evaluate(enum_variant_value, labels);
                if let CompilerValue {
                    kind: CompilerValueKind::EnumVariant(enum_variant),
                    share_info: _,
                } = enum_variant_value
                {
                    if let Some(branch) = branches.get(&enum_variant.label) {
                        if let CompilerValue {
                            kind: CompilerValueKind::Function(branch_fn),
                            share_info: _,
                        } = self.evaluate(branch, labels)
                        {
                            match enum_variant.value {
                                Some(value) => self.call(&branch_fn, [*value]),
                                None => self.call(&branch_fn, []),
                            }
                        } else {
                            compiler_error!(
                                value,
                                "Branch {} value {:?} was not function!",
                                enum_variant.label,
                                branch
                            )
                        }
                    } else {
                        compiler_error!(
                            value,
                            "Branch for enum variant \"{}\" not found",
                            enum_variant.label
                        )
                    }
                } else {
                    compiler_error!(
                        value,
                        "Value {:?} was not an enum variant",
                        enum_variant_value
                    )
                }
            }
        }
    }

    /// Evaluate a constraint, constraint and value must already be resolved as labels are
    /// not evaluated by this function
    pub fn evaluate_constraint(
        &self,
        constraint: &CompilerValue,
        value: CompilerValue,
    ) -> CompilerValue {
        if let CompilerValue {
            kind: CompilerValueKind::Function(constraint_fn),
            share_info: _,
        } = constraint
        {
            self.call(&constraint_fn, [value])
        } else {
            compiler_error!(
                constraint,
                "Constraint must be a function with one parameter, but was {:?}",
                constraint
            )
        }
    }
}

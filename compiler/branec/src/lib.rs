use std::collections::HashMap;

pub mod types;
use types::{Array, CompilerValue, CompilerValueKind, Error, Function};

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
    pub fn call(&self, function: &Function, args: &[CompilerValue]) -> CompilerValue {
        if args.len() != function.params.values.len() {
            return compiler_error!(
                function,
                "function call was expecting {} args, but was passed {} args",
                function.params.values.len(),
                args.len()
            );
        }

        let args: Vec<CompilerValue> = args
            .into_iter()
            .zip(function.params.values.iter())
            .map(|(arg, constraint)| self.evaluate_constraint(constraint, arg))
            .collect();

        // Did any of our constraints fail?
        for arg in args.iter() {
            if let CompilerValueKind::Error(_) = arg.kind {
                // TODO append stack trace
                return arg.clone();
            }
        }

        match &function.defintion {
            types::FunctionDefinition::Intrinsic(closure) => closure(&args),
            types::FunctionDefinition::Closure { operations, value } => {
                let mut labels = HashMap::new();
                for op in operations {
                    match op {
                        types::LabelOperation::Label(label, constraints, compiler_value) => {
                            let compiler_value = self.evaluate(compiler_value, &mut labels);

                            let compiler_value = match constraints {
                                Some(constraint) => {
                                    let constraint = self.evaluate(constraint, &mut labels);
                                    self.evaluate_constraint(&constraint, &compiler_value)
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
        match &value.kind {
            types::CompilerValueKind::Number(_) | types::CompilerValueKind::Error(_) => {
                value.clone()
            }
            types::CompilerValueKind::Label(label) => {
                println!("Evaluating label \"{}\"", label);
                if let Some(value) = labels.remove(label) {
                    value
                } else if let Some(intrinsic) = self.intrinsics.get(label) {
                    self.evaluate(intrinsic, labels)
                } else {
                    compiler_error!(value, "Label {} does not exist in this context", label)
                }
            }
            types::CompilerValueKind::Object(_object) => {
                todo!("Implement object evaluation");
            }
            types::CompilerValueKind::Array(array) => CompilerValue {
                kind: CompilerValueKind::Array(Array {
                    values: array
                        .values
                        .iter()
                        .map(|v| self.evaluate(v, labels))
                        .collect(),
                }),
                share_info: None,
            },
            types::CompilerValueKind::EnumVariant(variant) => {
                println!("Evaluating enum variant {}", variant.label);
                CompilerValue::variant(
                    variant.label.clone(),
                    variant.value.as_ref().map(|v| self.evaluate(&v, labels)),
                )
            }
            types::CompilerValueKind::Function(function) => {
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
            types::CompilerValueKind::Call(function_value, args) => {
                println!("Evaluating call");
                let function_value = self.evaluate(function_value, labels);
                if let CompilerValue {
                    kind: CompilerValueKind::Function(function),
                    share_info: _,
                } = function_value
                {
                    let args = args
                        .iter()
                        .map(|arg| self.evaluate(arg, labels))
                        .collect::<Vec<_>>();
                    self.call(&function, &args)
                } else {
                    compiler_error!(
                        value,
                        "Value {:?} was not a function, and cannot be called",
                        function_value
                    )
                }
            }
            types::CompilerValueKind::Match(enum_variant_value, branches) => {
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
                                Some(value) => self.call(&branch_fn, &[(*value).clone()]),
                                None => self.call(&branch_fn, &[]),
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
    /// sometimes not in scope
    pub fn evaluate_constraint(
        &self,
        constraint: &CompilerValue,
        value: &CompilerValue,
    ) -> CompilerValue {
        if let CompilerValue {
            kind: CompilerValueKind::EnumVariant(enum_variant),
            share_info: _,
        } = constraint
        {
            match enum_variant.label.as_str() {
                "Some" => {
                    if let Some(constraint_value) = &enum_variant.value {
                        if let CompilerValue {
                            kind: CompilerValueKind::Function(constraint_fn),
                            share_info: _,
                        } = constraint_value.as_ref()
                        {
                            self.call(&constraint_fn, &[value.clone()])
                        } else {
                            compiler_error!(
                                constraint,
                                "Constraint must be a function with one parameter, but was {:?}",
                                constraint_value
                            )
                        }
                    } else {
                        CompilerValue::error(
                            format!("Expected Some variant to contain value"),
                            format!("Evaluating constraint {:?}", constraint),
                        )
                    }
                }
                "None" => value.clone(),
                _ => CompilerValue::error(
                    format!(
                        "Branch for enum variant \"{}\" not found",
                        enum_variant.label
                    ),
                    format!("Evaluating constraint {:?}", constraint),
                ),
            }
        } else {
            compiler_error!(
                constraint,
                "Constraint must be an Optional enum, but was {:?}",
                constraint
            )
        }
    }
}

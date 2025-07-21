use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    sync::Arc,
};

use crate::Interpreter;

#[derive(Clone, Debug, PartialEq)]
pub struct Number {
    pub value: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub label: String,
    pub value: Option<Box<CompilerValue>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub values: Vec<CompilerValue>,
}

impl Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.values
                .iter()
                .map(|v| format!("{}", v))
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap_or_default()
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectMember {
    pub label: String,
    pub value: CompilerValue,
}
impl ObjectMember {
    pub fn new(label: impl Into<String>, value: CompilerValue) -> Self {
        Self {
            label: label.into(),
            value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    members: Vec<ObjectMember>,
    key_index: HashMap<String, usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// The members of this array should be defined as:
    /// { label: String, constraints: Array<Function> }
    pub params: Array,
    /// Should be an array of constraints
    pub returns: Array,
    pub defintion: FunctionDefinition,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LabelOperation {
    /// Label single compiler value
    Label(String, Option<CompilerValue>, CompilerValue),
    /// Hoist and optionally rename struct membes
    Destructure(Vec<(String, Option<String>)>, CompilerValue),
}

#[derive(Clone)]
pub enum FunctionDefinition {
    Closure {
        /// Operations prepping labels to be used for return value
        operations: Vec<LabelOperation>,
        /// Final value expression to be returned, may refrence labels defined above
        value: Box<CompilerValue>,
    },
    Intrinsic(Arc<dyn Fn(Vec<CompilerValue>, &Interpreter) -> CompilerValue>),
}

impl PartialEq for FunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Closure {
                    operations: ops1,
                    value: v1,
                },
                Self::Closure {
                    operations: ops2,
                    value: v2,
                },
            ) => ops1.eq(ops2) && v1 == v2,
            (Self::Intrinsic(p1), Self::Intrinsic(p2)) => Arc::ptr_eq(p1, p2),
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub stack_trace: Vec<String>,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerValueKind {
    /// Reference to a previously defined value
    Label(String),
    /// Number constant
    Number(Number),
    /// A string literal, we don't use array here for both performance and because I'm lazy
    String(String),

    /// A labeled variant
    EnumVariant(EnumVariant),
    /// Array of values
    Array(Array),
    /// Object
    Object(Object),

    /// Callable function
    Function(Function),
    /// Evaluation of a function
    ///
    /// (Function | Label, args)
    Call(Box<CompilerValue>, Vec<CompilerValue>),
    /// Map an enum value
    Match(Box<CompilerValue>, HashMap<String, CompilerValue>),
    /// Error, this may be used as a value as well as directly defined.
    Error(Error),
    /// Previously consumed value, when a label is consumed we put this in it's place
    ///
    /// This is for debugging only, and will always trigger an error if evaluation is attempted
    ///
    /// TODO: Should contain metadata about *where* this was consumed from
    Consumed,
}

/// When a value is shared, we need to store some info about it
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShareInfo {
    /// The unique itentifier for the value being shared
    pub share_id: Option<usize>,
    /// Which particular instance or merged instances of a share this is
    /// We store multiple just in case two shares with the same share_id are merged,
    /// but were only indirect siblings, so we store all ids until they are paired with
    /// the correct sibling list value in a merge
    pub sibling_id: Option<Vec<usize>>,
    /// Which siblings this share has
    pub siblings: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct CompilerValue {
    pub kind: CompilerValueKind,
    pub share_info: Option<ShareInfo>,
}

impl CompilerValue {
    pub fn label(label: impl Into<String>) -> Self {
        Self {
            kind: CompilerValueKind::Label(label.into()),
            share_info: None,
        }
    }

    pub fn number(number: impl Into<f64>) -> Self {
        Self {
            kind: CompilerValueKind::Number(Number {
                value: number.into(),
            }),
            share_info: None,
        }
    }

    pub fn string(str: impl Into<String>) -> Self {
        Self {
            kind: CompilerValueKind::String(str.into()),
            share_info: None,
        }
    }

    pub fn variant(label: impl Into<String>, value: Option<CompilerValue>) -> Self {
        Self {
            kind: CompilerValueKind::EnumVariant(EnumVariant {
                label: label.into(),
                value: value.map(|value| Box::new(value)),
            }),
            share_info: None,
        }
    }

    pub fn object(members: impl IntoIterator<Item = ObjectMember>) -> Self {
        Self {
            kind: CompilerValueKind::Object(Object::new(members)),
            share_info: None,
        }
    }

    pub fn array(values: Vec<CompilerValue>) -> Self {
        Self {
            kind: CompilerValueKind::Array(Array { values }),
            share_info: None,
        }
    }

    pub fn error(msg: impl Into<String>, source: impl Into<String>) -> Self {
        Self {
            kind: CompilerValueKind::Error(Error {
                stack_trace: vec![source.into()],
                message: msg.into(),
            }),
            share_info: None,
        }
    }

    pub fn intrinsic_fn(
        params: impl IntoIterator<Item = (impl Into<String>, Vec<CompilerValue>)>,
        callback: impl Fn(Vec<CompilerValue>, &Interpreter) -> CompilerValue + 'static,
    ) -> Self {
        Self {
            kind: CompilerValueKind::Function(Function {
                params: Array {
                    values: params
                        .into_iter()
                        .map(|(label, constraints)| {
                            CompilerValue::object([
                                ObjectMember {
                                    label: "label".into(),
                                    value: Self::string(label.into()),
                                },
                                ObjectMember {
                                    label: "constraints".into(),
                                    value: Self::array(constraints),
                                },
                            ])
                        })
                        .collect(),
                },
                returns: Array { values: Vec::new() },
                defintion: FunctionDefinition::Intrinsic(Arc::new(callback)),
            }),
            share_info: None,
        }
    }

    pub fn lazy(callback: impl Fn() -> CompilerValue + 'static) -> CompilerValue {
        CompilerValue::intrinsic_fn(std::iter::empty::<(String, _)>(), move |_, _| callback())
    }

    pub fn is_number(&self) -> bool {
        match &self.kind {
            CompilerValueKind::Number(_) => true,
            _ => false,
        }
    }

    pub fn as_number(&self) -> Option<&Number> {
        match &self.kind {
            CompilerValueKind::Number(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match &self.kind {
            CompilerValueKind::String(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Array> {
        match &self.kind {
            CompilerValueKind::Array(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&Object> {
        match &self.kind {
            CompilerValueKind::Object(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_variant(&self) -> Option<&EnumVariant> {
        match &self.kind {
            CompilerValueKind::EnumVariant(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn to_variant(self) -> Option<EnumVariant> {
        match self.kind {
            CompilerValueKind::EnumVariant(inner) => Some(inner),
            _ => None,
        }
    }
}

impl PartialEq for CompilerValue {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Display for CompilerValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CompilerValueKind::Label(label) => write!(f, "Label {}", label),
            CompilerValueKind::Number(number) => write!(f, "{}", number.value),
            CompilerValueKind::String(string) => write!(f, "\"{}\"", string),
            CompilerValueKind::EnumVariant(enum_variant) => match &enum_variant.value {
                Some(value) => write!(f, "variant {}({})", enum_variant.label, value),
                None => write!(f, "variant {}", enum_variant.label),
            },
            CompilerValueKind::Array(array) => write!(f, "{}", array),
            CompilerValueKind::Object(object) => write!(
                f,
                "{{ {} }}",
                object
                    .members()
                    .iter()
                    .map(|v| format!("{}: {}", v.label, v.value))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            CompilerValueKind::Function(function) => write!(
                f,
                "fn ({}){}",
                function
                    .params
                    .values
                    .iter()
                    .map(|v| format!("{}", v))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default(),
                function
                    .returns
                    .values
                    .iter()
                    .map(|v| format!("{}", v))
                    .reduce(|a, b| format!("{} + {}", a, b))
                    .map(|r| format!(" => {}", r))
                    .unwrap_or_default(),
            ),
            CompilerValueKind::Call(function, args) => write!(
                f,
                "call({}, [{}])",
                function,
                args.iter()
                    .map(|v| format!("{}", v))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            CompilerValueKind::Match(condition, branches) => write!(
                f,
                "match {} {{ {} }}",
                condition,
                branches
                    .iter()
                    .map(|(label, value)| format!("{} => {}", label, value))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            CompilerValueKind::Error(error) => write!(f, "Error({})", error.message),
            CompilerValueKind::Consumed => write!(f, "[consumed]"),
        }
    }
}

impl Object {
    pub fn new(members: impl IntoIterator<Item = ObjectMember>) -> Self {
        let (key_index, members) = members.into_iter().enumerate().fold(
            (HashMap::new(), Vec::new()),
            |(mut key_index, mut members), (index, member)| {
                key_index.insert(member.label.clone(), index);
                members.push(member);
                (key_index, members)
            },
        );
        Self { key_index, members }
    }

    pub fn members(&self) -> &Vec<ObjectMember> {
        &self.members
    }

    pub fn members_mut(&mut self) -> impl Iterator<Item = (&String, &mut CompilerValue)> {
        self.members.iter_mut().map(|m| (&m.label, &mut m.value))
    }

    pub fn unpack(self) -> impl Iterator<Item = (String, CompilerValue)> {
        self.members.into_iter().map(|m| (m.label, m.value))
    }

    pub fn get(&self, label: &str) -> Option<&ObjectMember> {
        if let Some(index) = self.key_index.get(label) {
            self.members.get(*index)
        } else {
            None
        }
    }

    pub fn push(&mut self, member: ObjectMember) {
        self.key_index
            .insert(member.label.clone(), self.members.len());
        self.members.push(member);
    }

    pub fn remove(&mut self, label: &str) -> Option<ObjectMember> {
        if let Some(index) = self.key_index.get(label).cloned() {
            let value = self.members.remove(index);
            self.key_index.remove(label);
            for (_key, i) in self.key_index.iter_mut() {
                if *i > index {
                    *i -= 1;
                }
            }
            Some(value)
        } else {
            None
        }
    }
}

impl Debug for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionDefinition::Closure { operations, value } => {
                write!(f, "Closure({:?}, {:?})", operations, value)
            }
            FunctionDefinition::Intrinsic(_) => write!(f, "intrinsic"),
        }
    }
}

pub trait CompilerValueApi: Sized {
    fn to_value(&self) -> CompilerValue;
    fn from_value(value: CompilerValue) -> Result<Self, Error>;
    fn spec() -> CompilerValue;
}

impl CompilerValueApi for CompilerValue {
    fn to_value(&self) -> CompilerValue {
        self.clone()
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        Ok(value)
    }

    fn spec() -> CompilerValue {
        CompilerValue::variant("Any", None)
    }
}

macro_rules! impl_compiler_value_alias_for_number {
    ($($t:ty),*) => {
        $(
            impl CompilerValueApi for $t {
                fn to_value(&self) -> CompilerValue {
                    CompilerValue::number(*self as f64)
                }

                fn from_value(value: CompilerValue) -> Result<Self, Error> {
                    if let Some(number) = value.as_number() {
                        Ok(number.value as Self)
                    } else {
                        Err(Error {
                            stack_trace: vec![],
                            message: format!("Expected a number but found {:?}", value),
                        })
                    }
                }

                fn spec() -> CompilerValue {
                    CompilerValue::variant("Number", None)
                }
            }
        )*
    };
}

impl_compiler_value_alias_for_number!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64);

impl CompilerValueApi for String {
    fn to_value(&self) -> CompilerValue {
        CompilerValue::string(self)
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        if let Some(string) = value.as_string() {
            Ok(string.clone())
        } else {
            Err(Error {
                stack_trace: vec![],
                message: format!("Expected a string but found {:?}", value),
            })
        }
    }

    fn spec() -> CompilerValue {
        CompilerValue::variant("String", None)
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Box<T> {
    fn to_value(&self) -> CompilerValue {
        (**self).to_value()
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        Ok(Box::new(T::from_value(value)?))
    }

    fn spec() -> CompilerValue {
        T::spec()
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Vec<T> {
    fn to_value(&self) -> CompilerValue {
        CompilerValue::array(self.iter().map(|item| item.to_value()).collect())
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        if let Some(array) = value.as_array() {
            array
                .values
                .iter()
                .cloned()
                .map(T::from_value)
                .collect::<Result<Vec<_>, _>>()
        } else {
            Err(Error {
                stack_trace: vec![],
                message: format!("Expected an array but found {:?}", value),
            })
        }
    }

    fn spec() -> CompilerValue {
        CompilerValue::variant(
            "Array",
            Some(CompilerValue::object([
                ObjectMember::new("spec", CompilerValue::variant("Some", Some(T::spec()))),
                ObjectMember::new("len", CompilerValue::variant("None", None)),
            ])),
        )
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Option<T> {
    fn to_value(&self) -> CompilerValue {
        match self {
            Some(value) => CompilerValue::variant("Some", Some(value.to_value())),
            None => CompilerValue::variant("None", None),
        }
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        if let Some(variant) = value.to_variant() {
            match variant.label.as_str() {
                "Some" => match variant.value {
                    Some(value) => Ok(Some(T::from_value(*value)?)),
                    None => Err(Error {
                        stack_trace: vec![],
                        message:
                            "Expected Some variant of Option to contain value, but it was empty"
                                .into(),
                    }),
                },
                "None" => Ok(None),
                label => Err(Error {
                    stack_trace: vec![],
                    message: format!("Expected an Option variant, but found {} variant", label),
                }),
            }
        } else {
            Err(Error {
                stack_trace: vec![],
                message: format!("Expected an EnumVariant"),
            })
        }
    }

    fn spec() -> CompilerValue {
        CompilerValue::variant(
            "Options",
            Some(CompilerValue::array(vec![
                CompilerValue::variant(
                    "EnumVariant",
                    Some(CompilerValue::object([
                        ObjectMember::new("label", CompilerValue::string("Some")),
                        ObjectMember::new(
                            "spec",
                            CompilerValue::variant("Some", Some(CompilerValue::lazy(|| T::spec()))),
                        ),
                    ])),
                ),
                CompilerValue::variant(
                    "EnumVariant",
                    Some(CompilerValue::object([
                        ObjectMember::new("label", CompilerValue::string("None")),
                        ObjectMember::new("spec", CompilerValue::variant("None", None)),
                    ])),
                ),
            ])),
        )
    }
}

impl<T: CompilerValueApi, E: CompilerValueApi> CompilerValueApi for Result<T, E> {
    fn to_value(&self) -> CompilerValue {
        match self {
            Ok(value) => CompilerValue::variant("Ok", Some(value.to_value())),
            Err(error) => CompilerValue::variant("Err", Some(error.to_value())),
        }
    }

    fn from_value(value: CompilerValue) -> Result<Self, Error> {
        if let Some(variant) = value.to_variant() {
            match variant.label.as_str() {
                "Ok" => match variant.value {
                    Some(value) => Ok(Ok(T::from_value(*value)?)),
                    None => Err(Error {
                        stack_trace: vec![],
                        message: "Expected Ok variant of Result to contain value, but it was empty"
                            .into(),
                    }),
                },
                "Err" => match variant.value {
                    Some(error) => Ok(Err(E::from_value(*error)?)),
                    None => Err(Error {
                        stack_trace: vec![],
                        message:
                            "Expected Err variant of Result to contain error, but it was empty"
                                .into(),
                    }),
                },
                label => Err(Error {
                    stack_trace: vec![],
                    message: format!("Expected a Result variant, but found {} variant", label),
                }),
            }
        } else {
            Err(Error {
                stack_trace: vec![],
                message: "Expected an EnumVariant".into(),
            })
        }
    }

    fn spec() -> CompilerValue {
        CompilerValue::variant(
            "Options",
            Some(CompilerValue::array(vec![
                CompilerValue::variant(
                    "EnumVariant",
                    Some(CompilerValue::object([
                        ObjectMember::new("label", CompilerValue::string("Ok")),
                        ObjectMember::new(
                            "spec",
                            CompilerValue::variant("Some", Some(CompilerValue::lazy(|| T::spec()))),
                        ),
                    ])),
                ),
                CompilerValue::variant(
                    "EnumVariant",
                    Some(CompilerValue::object([
                        ObjectMember::new("label", CompilerValue::string("Err")),
                        ObjectMember::new(
                            "spec",
                            CompilerValue::variant("None", Some(CompilerValue::lazy(|| E::spec()))),
                        ),
                    ])),
                ),
            ])),
        )
    }
}

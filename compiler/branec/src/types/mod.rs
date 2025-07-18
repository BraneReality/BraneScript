use std::{
    collections::HashMap,
    fmt::{self, Debug},
    sync::Arc,
};

#[derive(Clone, Debug)]
pub struct Number {
    pub value: f64,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub label: String,
    pub value: Option<Box<CompilerValue>>,
}

#[derive(Clone, Debug)]
pub struct Array {
    pub values: Vec<CompilerValue>,
}

#[derive(Clone, Debug)]
pub struct ObjectMember {
    pub label: String,
    pub value: CompilerValue,
}

#[derive(Clone, Debug)]
pub struct Object {
    members: Vec<ObjectMember>,
    key_index: HashMap<String, usize>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub params: Array,
    pub defintion: FunctionDefinition,
}

#[derive(Clone, Debug)]
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
    Intrinsic(Arc<dyn Fn(&[CompilerValue]) -> CompilerValue>),
}

#[derive(Clone, Debug)]
pub struct Error {
    pub stack_trace: Vec<String>,
    pub message: String,
}

#[derive(Clone, Debug)]
pub enum CompilerValueKind {
    /// Reference to a previously defined value
    Label(String),
    /// Number constant
    Number(Number),
    /// A labeled variant
    EnumVariant(EnumVariant),
    /// Array of values
    Array(Array),
    /// Object
    Object(Object),
    /// Callable function
    Function(Function),
    /// Result of a call to a function
    ///
    /// (Function | Label, args)
    Call(Box<CompilerValue>, Vec<CompilerValue>),
    /// Map an enum value
    Match(Box<CompilerValue>, HashMap<String, CompilerValue>),
    /// Error
    Error(Error),
}

/// When a value is shared, we need to store some info about it
#[derive(Clone, Debug)]
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

    pub fn variant(label: impl Into<String>, value: Option<CompilerValue>) -> Self {
        Self {
            kind: CompilerValueKind::EnumVariant(EnumVariant {
                label: label.into(),
                value: value.map(|value| Box::new(value)),
            }),
            share_info: None,
        }
    }

    pub fn error(msg: impl Into<String>, source: impl Into<String>) -> CompilerValue {
        Self {
            kind: CompilerValueKind::Error(Error {
                stack_trace: vec![source.into()],
                message: msg.into(),
            }),
            share_info: None,
        }
    }

    pub fn is_number(&self) -> bool {
        match &self.kind {
            CompilerValueKind::Number(_) => true,
            _ => false,
        }
    }
}

impl Object {
    pub fn members(&self) -> &Vec<ObjectMember> {
        &self.members
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

/*
impl Number {
    pub fn dump<T: std::fmt::Write>(&self, output: &mut T) -> fmt::Result {
        write!(output, "{}", self.value)
    }
}

impl EnumVariant {
    pub fn dump<T: std::fmt::Write>(&self, output: &mut T) -> fmt::Result {
        write!(output, "{}(", self.label)?;
        self.value.dump(output);
        write!(output, ")")
    }
}

impl Array {
    pub fn dump<T: std::fmt::Write>(&self, output: &mut T) -> fmt::Result {
        write!(
            output,
            "[{}]",
            self.values
                .iter()
                .map(|v| {
                    let mut s = String::new();
                    v.dump(s);
                    s
                })
                .reduce(|a, b| format!("{}, {}", a, b))
        )
    }
}

impl Object {
    pub fn dump<T: std::fmt::Write>(&self, output: &mut T) -> fmt::Result {
        write!("{");
        write!(
            output,
            "[{}]",
            self.values
                .iter()
                .map(|v| {
                    let mut s = String::new();
                    v.dump(s);
                    s
                })
                .reduce(|a, b| format!("{}, {}", a, b))
        )
    }
}*/

use defer::defer;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display},
    ops::Add,
    sync::Arc,
};

use crate::Interpreter;

#[derive(Clone, Debug, PartialEq)]
pub struct RangeBound {
    pub inclusive: bool,
    pub value: NumberValue,
}

#[derive(Debug, PartialEq)]
pub enum RangeComparison {
    Below,
    Within,
    Above,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumberRange {
    pub begin: RangeBound,
    pub end: RangeBound,
}

impl NumberRange {
    pub fn contains(&self, value: &NumberValue) -> bool {
        (if self.begin.inclusive {
            self.begin.value <= *value
        } else {
            self.begin.value < *value
        } && {
            if self.end.inclusive {
                *value <= self.end.value
            } else {
                *value < self.end.value
            }
        })
    }

    pub fn compare(&self, value: &NumberValue) -> RangeComparison {
        if (self.end.inclusive && *value > self.end.value)
            || (!self.end.inclusive && *value >= self.end.value)
        {
            RangeComparison::Above
        } else if (self.begin.inclusive && *value < self.begin.value)
            || (!self.begin.inclusive && *value <= self.begin.value)
        {
            RangeComparison::Below
        } else {
            RangeComparison::Within
        }
    }

    fn contains_range(&self, target: &NumberRange) -> bool {
        let start_ok = match self.begin.value.partial_cmp(&target.begin.value) {
            Some(std::cmp::Ordering::Less) => true,
            Some(std::cmp::Ordering::Equal) => self.begin.inclusive || !target.begin.inclusive,
            _ => false,
        };

        let end_ok = match self.end.value.partial_cmp(&target.end.value) {
            Some(std::cmp::Ordering::Greater) => true,
            Some(std::cmp::Ordering::Equal) => self.end.inclusive || !target.end.inclusive,
            _ => false,
        };

        start_ok && end_ok
    }
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct NumberRangeSet {
    ranges: Vec<NumberRange>,
}

impl NumberRangeSet {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn contains(&self, value: &NumberValue) -> bool {
        self.binary_search_contains(value, 0, self.ranges.len())
    }

    fn binary_search_contains(&self, value: &NumberValue, start: usize, end: usize) -> bool {
        if start >= end {
            return false;
        }
        let mid = (start + end) / 2;
        let range = &self.ranges[mid];

        match range.compare(value) {
            RangeComparison::Within => true,
            RangeComparison::Below => {
                if mid == 0 {
                    false
                } else {
                    self.binary_search_contains(value, start, mid)
                }
            }
            RangeComparison::Above => self.binary_search_contains(value, mid + 1, end),
        }
    }

    pub fn contains_set(&self, other: &NumberRangeSet) -> bool {
        let mut self_idx = 0;

        for other_range in &other.ranges {
            // Advance through self until we find a candidate that could contain other_range
            while self_idx < self.ranges.len() {
                let base_range = &self.ranges[self_idx];

                // Check if base_range ends before other_range starts
                if base_range.end.value < other_range.begin.value
                    || (base_range.end.value == other_range.begin.value
                        && (!base_range.end.inclusive || !other_range.begin.inclusive))
                {
                    self_idx += 1;
                    continue;
                }

                // Now check whether other_range is completely within base_range
                if base_range.contains_range(other_range) {
                    break; // This other_range is contained, check next
                } else {
                    return false;
                }
            }

            // If we run out of base ranges before containing all other ranges
            if self_idx >= self.ranges.len() {
                return false;
            }
        }

        true
    }

    pub fn constrain_lt(&mut self, value: NumberValue) {
        self.ranges
            .retain_mut(|range| match range.end.value.partial_cmp(&value) {
                Some(std::cmp::Ordering::Less) => true,
                Some(std::cmp::Ordering::Equal) => {
                    range.end.value = value.clone();
                    range.end.inclusive = false;
                    range.begin.value < value
                }
                Some(std::cmp::Ordering::Greater) => {
                    if range.begin.value < value {
                        range.end.value = value.clone();
                        range.end.inclusive = false;
                        true
                    } else {
                        false
                    }
                }
                None => false,
            });
    }

    pub fn constrain_le(&mut self, value: NumberValue) {
        self.ranges
            .retain_mut(|range| match range.end.value.partial_cmp(&value) {
                Some(std::cmp::Ordering::Less) => true,
                Some(std::cmp::Ordering::Equal) => {
                    range.end.value = value.clone();
                    range.end.inclusive = true;
                    range.begin.value <= value
                }
                Some(std::cmp::Ordering::Greater) => {
                    if range.begin.value <= value {
                        range.end.value = value.clone();
                        range.end.inclusive = true;
                        true
                    } else {
                        false
                    }
                }
                None => false,
            });
    }

    pub fn constrain_gt(&mut self, value: NumberValue) {
        self.ranges
            .retain_mut(|range| match range.begin.value.partial_cmp(&value) {
                Some(std::cmp::Ordering::Greater) => true,
                Some(std::cmp::Ordering::Equal) => {
                    range.begin.value = value.clone();
                    range.begin.inclusive = false;
                    range.end.value > value
                }
                Some(std::cmp::Ordering::Less) => {
                    if range.end.value > value {
                        range.begin.value = value.clone();
                        range.begin.inclusive = false;
                        true
                    } else {
                        false
                    }
                }
                None => false,
            });
    }

    pub fn constrain_ge(&mut self, value: NumberValue) {
        self.ranges
            .retain_mut(|range| match range.begin.value.partial_cmp(&value) {
                Some(std::cmp::Ordering::Greater) => true,
                Some(std::cmp::Ordering::Equal) => {
                    range.begin.value = value.clone();
                    range.begin.inclusive = true;
                    range.end.value >= value
                }
                Some(std::cmp::Ordering::Less) => {
                    if range.end.value >= value {
                        range.begin.value = value.clone();
                        range.begin.inclusive = true;
                        true
                    } else {
                        false
                    }
                }
                None => false,
            });
    }

    pub fn constrain_ne(&mut self, value: NumberValue) {
        let mut new_ranges = Vec::with_capacity(self.ranges.len());
        for range in &self.ranges {
            if !range.contains(&value) {
                new_ranges.push(range.clone());
            } else {
                // Split the range into two (left and right)
                let mut left = range.clone();
                let mut right = range.clone();

                let cmp_start = left.begin.value.partial_cmp(&value);
                let cmp_end = right.end.value.partial_cmp(&value);

                let left_valid = match cmp_start {
                    Some(std::cmp::Ordering::Less) => {
                        left.end.value = value.clone();
                        left.end.inclusive = false;
                        left.begin.value < left.end.value
                    }
                    _ => false,
                };

                let right_valid = match cmp_end {
                    Some(std::cmp::Ordering::Greater) => {
                        right.begin.value = value.clone();
                        right.begin.inclusive = false;
                        right.begin.value < right.end.value
                    }
                    _ => false,
                };

                if left_valid {
                    new_ranges.push(left);
                }
                if right_valid {
                    new_ranges.push(right);
                }
            }
        }
        self.ranges = new_ranges;
    }
}

impl Display for NumberRangeSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Ranges({})",
            self.ranges
                .iter()
                .map(|r| r.to_string())
                .reduce(|a, b| { format!("{} | {}", a, b) })
                .unwrap_or_default()
        )
    }
}

impl From<NumberRange> for NumberRangeSet {
    fn from(range: NumberRange) -> Self {
        NumberRangeSet {
            ranges: vec![range],
        }
    }
}

impl Display for NumberRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}..{}{}",
            if self.begin.inclusive { "[" } else { "(" },
            self.begin.value,
            self.end.value,
            if self.end.inclusive { "]" } else { ")" },
        )
    }
}

#[derive(Clone, Debug)]
pub enum NumberValue {
    Int(i128),
    Float(f64),
}

impl PartialEq for NumberValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NumberValue::Int(value), NumberValue::Int(other)) => value == other,
            (NumberValue::Int(i), NumberValue::Float(f))
            | (NumberValue::Float(f), NumberValue::Int(i)) => *i as f64 == *f,
            (NumberValue::Float(value), NumberValue::Float(other)) => value == other,
        }
    }
}

impl PartialOrd for NumberValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (NumberValue::Int(a), NumberValue::Int(b)) => a.partial_cmp(b),
            (NumberValue::Int(a), NumberValue::Float(b)) => (*a as f64).partial_cmp(b),
            (NumberValue::Float(a), NumberValue::Int(b)) => a.partial_cmp(&(*b as f64)),
            (NumberValue::Float(a), NumberValue::Float(b)) => a.partial_cmp(b),
        }
    }
}

impl Display for NumberValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumberValue::Int(value) => write!(f, "{}", value),
            NumberValue::Float(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NumberState {
    Const(NumberValue),
    /// All possible values of this number, as composed as a series of sorted ascending
    /// non-overlapping ranges
    Range(NumberRangeSet),
}

impl Display for NumberState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            NumberState::Const(number_value) => match number_value {
                NumberValue::Int(value) => write!(f, "{}", value),
                NumberValue::Float(value) => write!(f, "{}", value),
            },
            NumberState::Range(range_set) => write!(f, "{}", range_set),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberBitWidth {
    Unresolved,
    /// 8 bits
    BYTE,
    /// 16 bits
    WORD,
    /// 32 bits
    DWORD,
    /// 64 bits
    QWORD,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StorageType {
    Unresolved,
    Int,
    Float,
    //Vector Xmm >:) eventually?
}

#[derive(Clone, Debug, PartialEq)]
pub struct Number {
    pub value: NumberState,
    pub bit_width: NumberBitWidth,
    pub storage_type: StorageType,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Number({})", self.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    /// For use in initalizing, which value is this?
    pub label: String,
    /// Enum variants may or may not contain data.
    pub value: Option<Value>,
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.label)?;
        if let Some(value) = &self.value {
            write!(f, "({})", value)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enum {
    pub label: Option<String>,
    /// All possible variants that this value might possibly contain
    /// If only one, then that's the value that's contained
    pub variants: Vec<EnumVariant>,
}

impl Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{}", label)?;
        }
        write!(
            f,
            "({})",
            self.variants
                .iter()
                .map(|v| v.to_string())
                .reduce(|a, b| format!("{} | {}", a, b))
                .unwrap_or_default()
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayState {
    Const(Vec<Value>),
    Variable(Option<NumberState>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    /// Arrays must contain the same element type, if no element type is set, this is a tuple
    /// instead
    pub element_ty: Option<Box<Value>>,
    pub values: ArrayState,
}

impl Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            match &self.values {
                ArrayState::Const(values) => values
                    .iter()
                    .map(|v| format!("{}", v))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default(),
                ArrayState::Variable(len) => {
                    format!("{:?};{:?}", self.element_ty, len)
                }
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectMember {
    pub label: String,
    /// This object member may or may not exist.
    pub speculative: bool,
    pub value: Value,
}
impl ObjectMember {
    pub fn new(label: impl Into<String>, value: Value) -> Self {
        Self {
            label: label.into(),
            speculative: false,
            value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub label: Option<String>,
    members: Vec<ObjectMember>,
    key_index: HashMap<String, usize>,
    /// For use in type checking. Signals that extra members on object values checked against
    /// this one are not treated as errors.
    pub interface: bool,
}

impl Object {
    pub fn new(label: Option<String>, members: impl IntoIterator<Item = ObjectMember>) -> Self {
        let (key_index, members) = members.into_iter().enumerate().fold(
            (HashMap::new(), Vec::new()),
            |(mut key_index, mut members), (index, member)| {
                key_index.insert(member.label.clone(), index);
                members.push(member);
                (key_index, members)
            },
        );
        Self {
            label,
            key_index,
            interface: false,
            members,
        }
    }

    pub fn members(&self) -> &Vec<ObjectMember> {
        &self.members
    }

    pub fn members_mut(&mut self) -> impl Iterator<Item = (&String, &mut Value)> {
        self.members.iter_mut().map(|m| (&m.label, &mut m.value))
    }

    pub fn unpack(self) -> impl Iterator<Item = (String, Value)> {
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

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.members()
                .iter()
                .map(|v| format!(
                    "{}{}: {}",
                    v.label,
                    if v.speculative { "?" } else { "" },
                    v.value
                ))
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap_or_default()
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionParam {
    pub label: String,
    pub ty: Value,
}

impl Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.label, self.ty)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// The members of this array should be defined as:
    pub params: Vec<FunctionParam>,
    pub returns: Box<Value>,
    pub defintion: FunctionDefinition,
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn ({}) => {}",
            self.params
                .iter()
                .map(|v| v.to_string())
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap_or_default(),
            self.returns
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LabelOperation {
    /// Label single compiler value, optionally checked with a type value
    Label(String, Option<Expression>, Expression),
    /// Hoist and optionally rename struct members
    Destructure(Vec<(String, Option<String>)>, Expression),
}

#[derive(Clone)]
pub enum FunctionDefinition {
    Closure {
        /// Operations prepping labels to be used for return value
        operations: Vec<LabelOperation>,
        /// Final value expression to be returned, may refrence labels defined above
        value: Box<Expression>,
    },
    Intrinsic(Arc<dyn Fn(Vec<Value>, &Interpreter) -> Result<Value, Vec<Error>> + Send + Sync>),
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

impl Error {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            stack_trace: Vec::new(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error(\"{}\")", self.message)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// # Runtime valid values

    /// Number constant
    Number(Number),
    /// A labeled variant
    Enum(Enum),
    /// Array of values
    Array(Array),
    /// Object
    Object(Object),
    /// Callable function
    Function(Function),

    /// # Compile time valid values

    /// Insert the parent struct or enum that has this same label
    Recursive(String),
    /// The contained value *might* be any of these values
    Options(Vec<Value>),
    /// Widlcard that represents "this could be anything"
    Any,
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

impl Value {
    /// Checks if a value is contained within the description of this value
    pub fn contains(&self, value: &Value) -> Result<(), Vec<Error>> {
        let mut parents = Vec::new();
        self.contains_recursive(value, &mut parents)
    }

    fn contains_recursive<'t>(
        &'t self,
        value: &Value,
        parents: &mut Vec<(&'t str, &'t Value)>,
    ) -> Result<(), Vec<Error>> {
        use Value::*;

        match (self, value) {
            (Number(ty), Number(value)) => {
                match (&ty.value, &value.value) {
                    (NumberState::Const(ty), NumberState::Const(value)) => {
                        if ty == value {
                            Ok(())
                        } else {
                            Err(Error::new(format!(
                                "Type required constant Number, but found {}",
                                value
                            )))
                        }
                    }
                    (NumberState::Const(ty), NumberState::Range(value)) => Err(Error::new(
                        format!("Type required constant {}, but found {}", ty, value),
                    )),
                    (NumberState::Range(ty), NumberState::Const(value)) => {
                        if ty.contains(value) {
                            Ok(())
                        } else {
                            Err(Error::new(format!(
                                "Type required value in {} but found {}",
                                ty, value
                            )))
                        }
                    }
                    (NumberState::Range(ty), NumberState::Range(value)) => {
                        if ty.contains_set(value) {
                            Ok(())
                        } else {
                            Err(Error::new(format!(
                                "Type required value in {} but found {}",
                                ty, value
                            )))
                        }
                    }
                }
                .map_err(|e| vec![e])?;

                match &ty.bit_width {
                    NumberBitWidth::Unresolved => Ok(()),
                    width => {
                        if *width == value.bit_width {
                            Ok(())
                        } else {
                            Err(Error::new(format!(
                                "Expected bit width of {:?}, but found {:?}",
                                width, value.bit_width
                            )))
                        }
                    }
                }
                .map_err(|e| vec![e])?;

                match (&ty.storage_type, &value.storage_type) {
                    (StorageType::Unresolved, _) => Ok(()),
                    (ty, value) => Err(Error::new(format!(
                        "Expected storage type of {:?} but found {:?}",
                        ty, value
                    ))),
                }
                .map_err(|e| vec![e])?;

                Ok(())
            }
            (Enum(ty), Enum(value)) => {
                if let (Some(ty_label), Some(value_label)) = (&ty.label, &value.label) {
                    if ty_label != value_label {
                        return Err(vec![Error::new(format!(
                            "Expected enum {}, but found enum {}",
                            ty_label, value_label
                        ))]);
                    }
                }

                for variant in value.variants.iter() {
                    match ty.variants.iter().find(|ty_v| variant.label == ty_v.label) {
                        Some(v_ty) => match (&v_ty.value, &variant.value) {
                            (None, None) => Ok(()),
                            (None, Some(_)) => {
                                Err(vec![Error::new(format!("Expected a varaint with no data"))])
                            }

                            (Some(_), None) => {
                                Err(vec![Error::new(format!("Expected a varaint with data"))])
                            }
                            (Some(data_ty), Some(data_value)) => {
                                if let Some(label) = &ty.label {
                                    parents.push((label.as_str(), self));
                                }
                                let res = data_ty.contains_recursive(data_value, parents);
                                if let Some(label) = &ty.label {
                                    let stack = parents.pop();
                                    assert_eq!(Some((label.as_str(), self)), stack);
                                }
                                res
                            }
                        },
                        None => Err(vec![Error::new(format!(
                            "Expected a varaint of {} but found {}",
                            ty, variant
                        ))]),
                    }?;
                }
                Ok(())
            }
            (Array(ty), Array(value)) => {
                let ty_form = if ty.element_ty.is_some() {
                    "Array"
                } else {
                    "Tuple"
                };
                let value_form = if value.element_ty.is_some() {
                    "Array"
                } else {
                    "Tuple"
                };
                match (&ty.values, &value.values) {
                    (ArrayState::Const(ty_values), ArrayState::Const(value_values)) => {
                        if ty_values.len() != value_values.len() {
                            Err(vec![Error::new(format!(
                                "Expected {} of length {} but found {} of length {}",
                                ty_form,
                                ty_values.len(),
                                value_form,
                                value_values.len()
                            ))])
                        } else {
                            for (ty, value) in ty_values.iter().zip(value_values.iter()) {
                                ty.contains(value)?
                            }
                            Ok(())
                        }
                    }
                    (ArrayState::Const(_), ArrayState::Variable(_)) => {
                        Err(vec![Error::new(format!(
                            "Expected fixed length {}, but found unresolved {}",
                            ty_form, value_form
                        ))])
                    }
                    (ArrayState::Variable(ty_len), ArrayState::Const(values)) => {
                        if let Some(ty_len) = ty_len {
                            if !match ty_len {
                                NumberState::Const(ty_len) => {
                                    *ty_len == NumberValue::Int(values.len() as i128)
                                }
                                NumberState::Range(ty_len) => {
                                    ty_len.contains(&NumberValue::Int(values.len() as i128))
                                }
                            } {
                                return Err(vec![Error::new(format!(
                                    "Expected Array of length {} but found Array of length {}",
                                    ty_len,
                                    values.len()
                                ))]);
                            }
                        }
                        Ok(())
                    }
                    (ArrayState::Variable(ty), ArrayState::Variable(value)) => match (ty, value) {
                        (None, None) => Ok(()),
                        (None, Some(_)) => Ok(()),
                        (Some(len), None) => Err(vec![Error::new(format!(
                            "Expected {} of length {}, but found {} with unresolved length",
                            ty_form, len, value_form
                        ))]),
                        (Some(ty_len), Some(value_len)) => {
                            if ty_len == value_len {
                                Ok(())
                            } else {
                                Err(vec![Error::new(format!(
                                    "Expected {} of length {}, but found {} of length {}",
                                    ty_form, ty_len, value_form, value_len
                                ))])
                            }
                        }
                    },
                }
            }
            (Object(ty), Object(value)) => {
                let mut extra_members: HashSet<_> =
                    value.members().iter().map(|m| &m.label).collect();
                let mut errors = Vec::new();

                if let Some(label) = &ty.label {
                    parents.push((label.as_str(), self));
                }

                for m_ty in ty.members.iter() {
                    if let Some(member) = value.get(&m_ty.label) {
                        if let Err(mut err) = m_ty.value.contains_recursive(&member.value, parents)
                        {
                            errors.append(&mut err);
                        }
                        extra_members.remove(&member.label);
                    } else {
                        errors.push(Error {
                            stack_trace: Vec::new(),
                            message: format!("Object missing \"{}\" member", m_ty.label),
                        });
                    }
                }

                if let Some(label) = &ty.label {
                    let stack = parents.pop();
                    assert_eq!(Some((label.as_str(), self)), stack);
                }

                if !ty.interface && !extra_members.is_empty() {
                    errors.push(Error {
                        stack_trace: Vec::new(),
                        message: format!(
                            "Object has extra members: {}",
                            extra_members
                                .iter()
                                .map(|s| (*s).clone())
                                .reduce(|a, b| format!("{}, {}", a, b))
                                .unwrap_or_default()
                        ),
                    });
                }
                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
            (Function(ty), Function(value)) => {
                if ty.params.len() != value.params.len() {
                    return Err(vec![Error::new(format!(
                        "Expecting a function that takes {} params, but got one that takes {}",
                        ty.params.len(),
                        value.params.len()
                    ))]);
                }

                let mut errors = Vec::new();

                for (ty, value) in ty.params.iter().zip(value.params.iter()) {
                    if let Err(mut err) = ty.ty.contains_recursive(&value.ty, parents) {
                        errors.append(&mut err);
                    }
                }

                if let Err(mut err) = ty.returns.contains_recursive(&value.returns, parents) {
                    errors.append(&mut err)
                }

                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
            (Recursive(ty), Recursive(value)) => {
                if ty == value {
                    Ok(())
                } else {
                    Err(vec![Error::new(format!(
                        "Type required {} but found {}",
                        ty, value
                    ))])
                }
            }
            (Recursive(ty), value) => {
                let parent = parents.iter().rev().find(|(label, _r)| label == ty);
                if let Some((_, ty)) = parent {
                    ty.contains_recursive(value, parents)
                } else {
                    Err(vec![Error::new(format!(
                        "Could not find recursive type parent {}",
                        ty
                    ))])
                }
            }
            (Options(types), value) => {
                for ty in types {
                    if let Ok(_) = ty.contains(value) {
                        return Ok(());
                    }
                }
                Err(vec![Error::new(format!(
                    "Expected any of {}, but found {}",
                    self, value
                ))])
            }
            (Any, _) => Ok(()),
            (ty, value) => Err(vec![Error::new(format!(
                "Type required {} but found {}",
                ty, value
            ))]),
        }
    }

    pub fn is_number(&self) -> bool {
        match &self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn as_number(&self) -> Option<&Number> {
        match &self {
            Value::Number(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Array> {
        match &self {
            Value::Array(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&Object> {
        match &self {
            Value::Object(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Enum> {
        match &self {
            Value::Enum(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn variant(
        label: Option<impl Into<String>>,
        variant: impl Into<String>,
        value: Option<Value>,
    ) -> Self {
        Value::Enum(Enum {
            label: label.map(|v| v.into()),
            variants: vec![EnumVariant {
                label: variant.into(),
                value,
            }],
        })
    }

    pub fn intrinsic_fn(
        params: impl IntoIterator<Item = (impl Into<String>, Value)>,
        callback: impl Fn(Vec<Value>, &Interpreter) -> Result<Value, Vec<Error>> + 'static + Send + Sync,
    ) -> Self {
        Value::Function(Function {
            params: params
                .into_iter()
                .map(|(label, ty)| FunctionParam {
                    label: label.into(),
                    ty,
                })
                .collect(),
            returns: Box::new(Value::Any),
            defintion: FunctionDefinition::Intrinsic(Arc::new(callback)),
        })
    }

    pub fn lazy(callback: impl Fn() -> Value + 'static + Send + Sync) -> Self {
        Self::intrinsic_fn(
            std::iter::empty::<(String, _)>(),
            move |_, _| Ok(callback()),
        )
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Value::Number(number) => write!(f, "{}", number),
            Value::Enum(value) => write!(f, "{}", value),
            Value::Array(array) => write!(f, "{}", array),
            Value::Object(object) => write!(f, "{}", object),
            Value::Function(function) => write!(f, "{}", function),
            Value::Recursive(rec) => write!(f, "[recursive({})]", rec),
            Value::Options(values) => write!(
                f,
                "[options]({})",
                values
                    .iter()
                    .map(|v| v.to_string())
                    .reduce(|a, b| format!("{} | {}", a, b))
                    .unwrap_or_default()
            ),
            Value::Any => write!(f, "*"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// A compiler or runtime value
    Value(Value),
    /// Reference to a previously defined value
    Label(String),
    /// Evaluation of a function
    ///
    /// (Function, args)
    Call(Box<Expression>, Vec<Expression>),
    /// Match based on types
    ///
    /// (condition, [type value, function accepting arg of type])
    Match(Box<Expression>, Vec<(Expression, Expression)>),
    /// Create a structured object, optionally enforcing a type
    ///
    /// (type value, [(label, speculative, member value)]
    Structure(Option<Box<Expression>>, Vec<(String, bool, Expression)>),
    /// Create Enum Variant
    ///
    /// (Template Enum Value, Variant Label, Variant Value)
    EnumVariant(Option<Box<Expression>>, String, Option<Box<Expression>>),

    /// Construct a function
    ///
    /// (params: [(label, type value)], return type value, body labels, body return)
    ConstructFunction(
        Vec<(String, Expression)>,
        Box<Expression>,
        Vec<LabelOperation>,
        Box<Expression>,
    ),
    /// Way to report errors down a value path
    Error(Vec<Error>),
    /// Previously consumed value, when a label is consumed we put this in it's place
    ///
    /// This is for debugging only, and will always trigger an error if evaluation is attempted
    ///
    /// TODO: Should contain metadata about *where* this was consumed from
    Consumed,
}

impl Expression {
    pub fn label(label: impl Into<String>) -> Self {
        Expression::Label(label.into())
    }

    pub fn number(number: impl Into<f64>) -> Self {
        Self::Value(Value::Number(Number {
            value: NumberState::Const(NumberValue::Float(number.into())),
            bit_width: NumberBitWidth::Unresolved,
            storage_type: StorageType::Unresolved,
        }))
    }

    pub fn variant(
        label: Option<impl Into<String>>,
        variant: impl Into<String>,
        value: Option<Value>,
    ) -> Self {
        Self::Value(Value::variant(label, variant, value))
    }

    pub fn object(members: impl IntoIterator<Item = ObjectMember>) -> Self {
        Self::Value(Value::Object(Object::new(None, members)))
    }

    pub fn array(values: Vec<Value>, element_ty: Value) -> Self {
        Self::Value(Value::Array(Array {
            values: ArrayState::Const(values),
            element_ty: Some(Box::new(element_ty)),
        }))
    }

    pub fn error(msg: impl Into<String>, source: impl Into<String>) -> Self {
        Self::Error(vec![Error {
            stack_trace: vec![source.into()],
            message: msg.into(),
        }])
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match &self {
            Label(label) => write!(f, "Label {}", label),
            Call(function, args) => write!(
                f,
                "call({}, [{}])",
                function,
                args.iter()
                    .map(|v| format!("{}", v))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            Match(condition, branches) => write!(
                f,
                "match {} {{ {} }}",
                condition,
                branches
                    .iter()
                    .map(|(label, value)| format!("{} => {}", label, value))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            Error(error) => write!(
                f,
                "{}",
                error
                    .iter()
                    .map(|e| e.to_string())
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            Consumed => write!(f, "[consumed]"),
            Value(value) => write!(f, "{}", value),
            Structure(expression, members) => write!(
                f,
                "{}{{ {} }}",
                expression
                    .as_ref()
                    .map(|e| e.to_string())
                    .unwrap_or_default(),
                members
                    .iter()
                    .map(|(label, speculative, value)| format!(
                        "{}{}: {}",
                        label,
                        if *speculative { "?" } else { "" },
                        value
                    ))
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_default()
            ),
            EnumVariant(validator, label, value) => {
                write!(
                    f,
                    "[{}].{}({})",
                    validator
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_default(),
                    label,
                    value.as_ref().map(|v| v.to_string()).unwrap_or_default()
                )
            }
            ConstructFunction(items, expression, label_operations, expression1) => todo!(),
        }
    }
}

pub trait CompilerValueApi: Sized {
    fn as_value(&self) -> Value;
    fn from_value(value: &Value) -> Result<Self, Error>;
    fn ty() -> Value;
}

impl CompilerValueApi for Value {
    fn as_value(&self) -> Value {
        self.clone()
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        Ok(value.clone())
    }

    fn ty() -> Value {
        Value::Any
    }
}

/*
impl CompilerValueApi for i32 {
    fn as_value(&self) -> Value {
        Value::Number(Number {
            value: NumberState::Const(NumberValue::Int(*self as isize)),
            bit_width: NumberBitWidth::DWORD,
            storage_type: StorageType::Int,
        })
    }

    fn from_value(value: Value) -> Result<Self, Error> {
        if let Value::Number(Number {
            value,
            bit_width,
            storage_type,
        }) = &value
        {
            match value {
                NumberState::Const(number_value) => Ok(match number_value {
                    NumberValue::Int(value) => *value as i32,
                    NumberValue::Float(value) => *value as i32,
                }),
                NumberState::Traits(number_traits) => Err(Error {
                    stack_trace: vec![],
                    message: "Cannot convert unresolved value to i32".into(),
                }),
            }
        } else {
            Err(Error {
                stack_trace: vec![],
                message: format!("Cannot convert {} value to i32", value),
            })
        }
    }

    fn ty() -> Value {
        Value::Number(Number {
            value: NumberState::Traits(vec![NumberTrait::Range(
                RangeBound {
                    inclusive: true,
                    value: NumberValue::Int(i32::MIN as i128),
                },
                RangeBound {
                    inclusive: true,
                    value: NumberValue::Int(i32::MIN as i128),
                },
            )]),
            bit_width: NumberBitWidth::DWORD,
            storage_type: StorageType::Int,
        })
    }
}
*/

macro_rules! impl_compiler_value_api_for_int {
    ($($int_type:ident)*) => {
        $(
            impl CompilerValueApi for $int_type {
                fn as_value(&self) -> Value {
                    Value::Number(Number {
                        value: NumberState::Const(NumberValue::Int(*self as i128)),
                        bit_width: match std::mem::size_of::<$int_type>() * 8 {
                            8 => NumberBitWidth::BYTE,
                            16 => NumberBitWidth::WORD,
                            32 => NumberBitWidth::DWORD,
                            64 => NumberBitWidth::QWORD,
                            _ => unreachable!(),
                        },
                        storage_type: StorageType::Int,
                    })
                }

                fn from_value(value: &Value) -> Result<Self, Error> {
                    if let Value::Number(Number {
                        value,
                        bit_width: _,
                        storage_type: _,
                    }) = &value
                    {
                        match value {
                            NumberState::Const(number_value) => Ok(match number_value {
                                NumberValue::Int(value) => *value as $int_type,
                                NumberValue::Float(_) => return Err(Error {
                                    stack_trace: vec![],
                                    message: "Cannot convert float to int".into(),
                                }),
                            }),
                            NumberState::Range(_) => Err(Error {
                                stack_trace: vec![],
                                message: "Cannot convert unresolved value to int".into(),
                            }),
                        }
                    } else {
                        Err(Error {
                            stack_trace: vec![],
                            message: format!("Cannot convert {} value to int", value),
                        })
                    }
                }

                fn ty() -> Value {
                    let bit_width = match std::mem::size_of::<$int_type>() * 8 {
                        8 => NumberBitWidth::BYTE,
                        16 => NumberBitWidth::WORD,
                        32 => NumberBitWidth::DWORD,
                        64 => NumberBitWidth::QWORD,
                        _ => unreachable!(),
                    };
                    Value::Number(Number {
                        value: NumberState::Range(NumberRange{
                            begin:RangeBound {
                                inclusive: true,
                                value: NumberValue::Int(<$int_type>::MIN as i128),
                            },
                            end:RangeBound {
                                inclusive: true,
                                value: NumberValue::Int(<$int_type>::MAX as i128),
                            },
                        }.into()),
                        bit_width,
                        storage_type: StorageType::Int,
                    })
                }
            }
        )*
    };
}

macro_rules! impl_compiler_value_api_for_float {
    ($($float_type:ident)*) => {
        $(
            impl CompilerValueApi for $float_type {
                fn as_value(&self) -> Value {
                    Value::Number(Number {
                        value: NumberState::Const(NumberValue::Float(*self as f64)),
                        bit_width: match std::mem::size_of::<$float_type>() * 8 {
                            32 => NumberBitWidth::DWORD,
                            64 => NumberBitWidth::QWORD,
                            _ => unreachable!(),
                        },
                        storage_type: StorageType::Float,
                    })
                }

                fn from_value(value: &Value) -> Result<Self, Error> {
                    if let Value::Number(Number {
                        value,
                        bit_width: _,
                        storage_type: _,
                    }) = &value
                    {
                        match value {
                            NumberState::Const(number_value) => Ok(match number_value {
                                NumberValue::Int(_) => return Err(Error {
                                    stack_trace: vec![],
                                    message: "Cannot convert int to float".into(),
                                }),
                                NumberValue::Float(value) => *value as $float_type,
                            }),
                            NumberState::Range(_) => Err(Error {
                                stack_trace: vec![],
                                message: "Cannot convert unresolved value to float".into(),
                            }),
                        }
                    } else {
                        Err(Error {
                            stack_trace: vec![],
                            message: format!("Cannot convert {} value to float", value),
                        })
                    }
                }

                fn ty() -> Value {
                    let bit_width = match std::mem::size_of::<$float_type>() * 8 {
                        32 => NumberBitWidth::DWORD,
                        64 => NumberBitWidth::QWORD,
                        _ => unreachable!(),
                    };
                    Value::Number(Number {
                        value: NumberState::Range(NumberRange{
                            begin: RangeBound {
                                inclusive: true,
                                value: NumberValue::Float(<$float_type>::MIN as f64),
                            },
                            end: RangeBound {
                                inclusive: true,
                                value: NumberValue::Float(<$float_type>::MAX as f64),
                            },
                        }.into()),
                        bit_width,
                        storage_type: StorageType::Float,
                    })
                }
            }
        )*
    };
}

impl_compiler_value_api_for_int!(i8 i16 i32 i64 isize u8 u16 u32 u64 usize);
impl_compiler_value_api_for_float!(f32 f64);

impl CompilerValueApi for String {
    fn as_value(&self) -> Value {
        Value::Array(Array {
            element_ty: Some(Box::new(u8::ty())),
            values: ArrayState::Const(self.as_bytes().iter().map(|b| b.as_value()).collect()),
        })
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        if let Value::Array(Array {
            values: ArrayState::Const(array),
            element_ty,
        }) = value
        {
            if element_ty
                .as_ref()
                .map(|e| **e == i8::ty())
                .unwrap_or(false)
            {
                return String::from_utf8(
                    array
                        .iter()
                        .map(|v| u8::from_value(v))
                        .collect::<Result<Vec<u8>, Error>>()?,
                )
                .map_err(|e| Error {
                    message: e.to_string(),
                    stack_trace: Vec::new(),
                });
            }
        }
        Err(Error {
            stack_trace: vec![],
            message: "Expected a constant character array".into(),
        })
    }

    fn ty() -> Value {
        Value::Array(Array {
            element_ty: Some(Box::new(u8::ty())),
            values: ArrayState::Variable(None),
        })
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Box<T> {
    fn as_value(&self) -> Value {
        (**self).as_value()
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        Ok(Box::new(T::from_value(value)?))
    }

    fn ty() -> Value {
        T::ty()
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Vec<T> {
    fn as_value(&self) -> Value {
        Value::Array(Array {
            values: ArrayState::Const(self.iter().map(|item| item.as_value()).collect()),
            element_ty: Some(Box::new(T::ty())),
        })
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        if let Some(array) = value.as_array() {
            match &array.values {
                ArrayState::Const(array) => {
                    return array
                        .iter()
                        .map(T::from_value)
                        .collect::<Result<Vec<_>, _>>()
                }
                ArrayState::Variable(_) => {}
            }
        };
        Err(Error {
            stack_trace: vec![],
            message: format!("Expected a const array but found {:?}", value),
        })
    }

    fn ty() -> Value {
        Value::Array(Array {
            element_ty: Some(Box::new(T::ty())),
            values: ArrayState::Variable(None),
        })
    }
}

impl<T: CompilerValueApi> CompilerValueApi for Option<T> {
    fn as_value(&self) -> Value {
        match self {
            Some(value) => Value::variant(Some("Option"), "Some", Some(value.as_value())),
            None => Value::variant(Some("Option"), "None", None),
        }
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        if let Some(e) = value.as_enum() {
            if e.variants.len() != 1 {
                return Err(Error {
                    stack_trace: Vec::new(),
                    message: format!("Tried to read enum without a resolved value"),
                });
            }
            let variant = e.variants.first().unwrap();
            match variant.label.as_str() {
                "Some" => match &variant.value {
                    Some(value) => Ok(Some(T::from_value(value)?)),
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

    fn ty() -> Value {
        Value::Enum(Enum {
            label: Some("Option".into()),
            variants: vec![
                EnumVariant {
                    label: "Some".into(),
                    value: Some(T::ty()),
                },
                EnumVariant {
                    label: "None".into(),
                    value: None,
                },
            ],
        })
    }
}

impl<T: CompilerValueApi, E: CompilerValueApi> CompilerValueApi for Result<T, E> {
    fn as_value(&self) -> Value {
        match self {
            Ok(value) => Value::variant(Some("Result"), "Ok", Some(value.as_value())),
            Err(error) => Value::variant(Some("Result"), "Err", Some(error.as_value())),
        }
    }

    fn from_value(value: &Value) -> Result<Self, Error> {
        if let Some(e) = value.as_enum() {
            if e.variants.len() != 1 {
                return Err(Error {
                    stack_trace: Vec::new(),
                    message: format!("Tried to read enum without a resolved value"),
                });
            }
            let variant = &e.variants[0];
            match variant.label.as_str() {
                "Ok" => match &variant.value {
                    Some(value) => Ok(Ok(T::from_value(value)?)),
                    None => Err(Error {
                        stack_trace: vec![],
                        message: "Expected Ok variant of Result to contain value, but it was empty"
                            .into(),
                    }),
                },
                "Err" => match &variant.value {
                    Some(error) => Ok(Err(E::from_value(error)?)),
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

    fn ty() -> Value {
        Value::Enum(Enum {
            label: Some("Result".into()),
            variants: vec![
                EnumVariant {
                    label: "Ok".into(),
                    value: Some(T::ty()),
                },
                EnumVariant {
                    label: "Err".into(),
                    value: Some(E::ty()),
                },
            ],
        })
    }
}

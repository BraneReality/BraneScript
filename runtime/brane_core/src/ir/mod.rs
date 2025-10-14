use anyhow::{anyhow, bail};

pub type StructId = u64;
pub type EnumId = u64;
pub type FnId = i64;
pub type PipeId = i64;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Ref {
    Struct(StructId),
    Enum(EnumId),
    Function(FnId),
    Pipeline(PipeId),
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum NativeType {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    F32,
    U64,
    I64,
    F64,
    U128,
    I128,
    /// (is mutable, inner ty)
    Ptr(bool, Option<Box<Ty>>),
}

impl NativeType {
    pub fn size(&self) -> usize {
        use NativeType::*;
        match self {
            Bool | I8 | U8 => 1,
            I16 | U16 => 2,
            I32 | U32 | F32 | Ptr(_, _) => 4,
            I64 | U64 | F64 => 8,
            U128 | I128 => 16,
        }
    }

    pub fn alignment(&self) -> usize {
        self.size()
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            NativeType::I8
                | NativeType::I16
                | NativeType::I32
                | NativeType::I64
                | NativeType::I128
                | NativeType::U8
                | NativeType::U16
                | NativeType::U32
                | NativeType::U64
                | NativeType::U128
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, NativeType::F32 | NativeType::F64)
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            NativeType::U8 | NativeType::U16 | NativeType::U32 | NativeType::U64 | NativeType::U128
        )
    }
}

impl Display for NativeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NativeType::Bool => write!(f, "bool"),
            NativeType::U8 => write!(f, "u8"),
            NativeType::I8 => write!(f, "i8"),
            NativeType::U16 => write!(f, "u16"),
            NativeType::I16 => write!(f, "i16"),
            NativeType::U32 => write!(f, "u32"),
            NativeType::I32 => write!(f, "i32"),
            NativeType::F32 => write!(f, "f32"),
            NativeType::U64 => write!(f, "u64"),
            NativeType::I64 => write!(f, "i64"),
            NativeType::F64 => write!(f, "f64"),
            NativeType::U128 => write!(f, "u128"),
            NativeType::I128 => write!(f, "i128"),
            NativeType::Ptr(is_mut, inner_ty) => {
                if let Some(ty) = inner_ty {
                    write!(f, "*{}{}", if *is_mut { "mut " } else { "" }, ty)
                } else {
                    write!(f, "*{}", if *is_mut { "mut " } else { "" })
                }
            }
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Ty {
    Native(NativeType),
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Value {
    PhiArg {
        block: u32,
        arg: u32,
    },
    FnArg(u32),
    VecFnArg(u32, u32),
    BlockOp {
        block: u32,
        op: u32,
    },
    /// Structs and enums are returned from function calls as vecs of values, accessed by index
    VecBlockOp {
        block: u32,
        op: u32,
        index: u32,
    },
    Const(ConstValue),
}

impl Value {
    pub fn const_bool(value: bool) -> Self {
        Value::Const(ConstValue::Bool(value))
    }

    pub fn const_u8(value: u8) -> Self {
        Value::Const(ConstValue::U8(value))
    }

    pub fn const_i8(value: i8) -> Self {
        Value::Const(ConstValue::I8(value))
    }

    pub fn const_u16(value: u16) -> Self {
        Value::Const(ConstValue::U16(value))
    }

    pub fn const_i16(value: i16) -> Self {
        Value::Const(ConstValue::I16(value))
    }

    pub fn const_i32(value: i32) -> Self {
        Value::Const(ConstValue::I32(value))
    }

    pub fn const_u32(value: u32) -> Self {
        Value::Const(ConstValue::U32(value))
    }

    pub fn const_f32(value: f32) -> Self {
        Value::Const(ConstValue::F32(value))
    }

    pub fn const_u64(value: u64) -> Self {
        Value::Const(ConstValue::U64(value))
    }

    pub fn const_i64(value: i64) -> Self {
        Value::Const(ConstValue::I64(value))
    }

    pub fn const_f64(value: f64) -> Self {
        Value::Const(ConstValue::F64(value))
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum ConstValue {
    Bool(bool),
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    F32(f32),
    U64(u64),
    I64(i64),
    F64(f64),
}

impl ConstValue {
    pub fn is_ty(&self, nativetype: &NativeType) -> bool {
        match (self, nativetype) {
            (ConstValue::Bool(_), NativeType::Bool) => true,
            (ConstValue::U8(_), NativeType::U8) => true,
            (ConstValue::I8(_), NativeType::I8) => true,
            (ConstValue::U16(_), NativeType::U16) => true,
            (ConstValue::I16(_), NativeType::I16) => true,
            (ConstValue::U32(_), NativeType::U32) => true,
            (ConstValue::I32(_), NativeType::I32) => true,
            (ConstValue::F32(_), NativeType::F32) => true,
            (ConstValue::U64(_), NativeType::U64) => true,
            (ConstValue::I64(_), NativeType::I64) => true,
            (ConstValue::F64(_), NativeType::F64) => true,
            _ => false,
        }
    }

    pub fn index_of_ty(value: usize, ty: &NativeType) -> anyhow::Result<Self> {
        Ok(match ty {
            NativeType::Bool => ConstValue::Bool(value != 0),
            NativeType::U8 => ConstValue::U8(value as u8),
            NativeType::I8 => ConstValue::I8(value as i8),
            NativeType::U16 => ConstValue::U16(value as u16),
            NativeType::I16 => ConstValue::I16(value as i16),
            NativeType::U32 => ConstValue::U32(value as u32),
            NativeType::I32 => ConstValue::I32(value as i32),
            NativeType::F32 => ConstValue::F32(value as f32),
            NativeType::U64 => ConstValue::U64(value as u64),
            NativeType::I64 => ConstValue::I64(value as i64),
            NativeType::F64 => ConstValue::F64(value as f64),
            _ => bail!("Unsupported type for index_of_ty"),
        })
    }
}

impl std::hash::Hash for ConstValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ConstValue::Bool(v) => v.hash(state),
            ConstValue::U8(v) => v.hash(state),
            ConstValue::I8(v) => v.hash(state),
            ConstValue::U16(v) => v.hash(state),
            ConstValue::I16(v) => v.hash(state),
            ConstValue::U32(v) => v.hash(state),
            ConstValue::I32(v) => v.hash(state),
            ConstValue::U64(v) => v.hash(state),
            ConstValue::I64(v) => v.hash(state),
            ConstValue::F32(v) => (*v as i64).hash(state),
            ConstValue::F64(v) => (*v as i64).hash(state),
        }
    }
}

impl Eq for ConstValue {}

#[derive(PartialEq, Clone, Debug)]
pub enum Op {
    Load {
        ty: NativeType,
        ptr: Value,
    },
    Store {
        src: Value,
        ptr: Value,
    },
    Unary {
        op: UnaryOp,
        value: Value,
    },
    Binary {
        op: BinaryOp,
        left: Value,
        right: Value,
    },
    // Function calls
    Call {
        func: i32,
        input: Vec<FnArg>,
    },
    CallIndirect {
        func_handle: Value,
        input: Vec<FnArg>,
    },
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum UnaryOp {
    INeg,
    FNeg,
    Alloc,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum BinaryOp {
    IAdd,
    FAdd,
    ISub,
    FSub,
    IMul,
    FMul,
    SDiv,
    UDiv,
    FDiv,
    URem,
    SRem,
    SCmp(CmpTy),
    UCmp(CmpTy),
    FCmp(CmpTy),
    And,
    Or,
    Xor,
    ShiftL,
    IShiftR,
    UShiftR,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CmpTy {
    Eq,
    Ne,
    Gt,
    Ge,
}

/// Block terminating operator
#[derive(Clone, PartialEq)]
pub enum TermOp {
    Jump {
        block: u32,
    },
    JumpIf {
        cond: Value,
        then: u32,
        else_: u32,
    },
    JumpMap {
        cond: Value,
        branches: Vec<(u128, u32)>,
        default: u32,
    },
    /// For regular returns, only return one value. For structs or enums, return a flattened value
    /// array
    Ret(Option<Vec<Value>>),
}

/// Values passed to functions
#[derive(Clone, PartialEq, Debug)]
pub enum FnArg {
    Value(Value),
    /// Flattened values of a struct or enum, offset/alignment will be taken from the function sig
    Obj(Vec<Value>),
}

#[derive(Clone, PartialEq)]
pub struct StructMember {
    pub id: Option<String>,
    pub ty: Ty,
}

#[derive(Clone, PartialEq)]
pub struct Struct {
    pub id: Option<String>,
    pub members: Vec<StructMember>,
    pub packed: bool,
}

pub struct StructLayout {
    // offsets of struct members in bytes
    pub byte_offsets: Vec<usize>,
    // total size of the struct
    pub size: usize,
    pub alignment: usize,
}

/// Increase a size or offset so that it alligns properly
pub fn pad_align(size: usize, align: usize) -> usize {
    if size == 0 {
        return size;
    }
    (size + align - 1) & !(size - 1) // Align offset
}

impl Struct {
    pub fn layout(&self, module: &Module) -> anyhow::Result<StructLayout> {
        let mut byte_offsets = Vec::with_capacity(self.members.len());
        let mut offset = 0;
        let mut max_alignment = 1;

        if self.members.is_empty() {
            return Ok(StructLayout {
                byte_offsets,
                size: 1,
                alignment: 1,
            });
        }

        for member in &self.members {
            let (size, align) = module.size_align(&member.ty)?;

            if !self.packed {
                offset = pad_align(offset, align); // Align offset
            }
            byte_offsets.push(offset);
            offset += size;
            if !self.packed {
                max_alignment = max_alignment.max(align);
            }
        }

        if !self.packed {
            offset = pad_align(offset, max_alignment); // Align total size
        }

        assert_ne!(0, offset, "structs with members cannot be zero sized!");

        Ok(StructLayout {
            byte_offsets,
            size: offset,
            alignment: max_alignment,
        })
    }
}

#[derive(Clone, PartialEq)]
pub struct EnumVariant {
    pub id: String,
    pub ty: Option<Ty>,
}

#[derive(Clone, PartialEq)]
pub struct Enum {
    pub id: Option<String>,
    pub variants: Vec<EnumVariant>,
    pub packed: bool,
}

pub struct EnumLayout {
    // total size of the struct
    pub union_size: usize,
    pub union_offset: usize,
    /// The size of the index_ty. For enums with only one or no variants index_ty will be none, and
    /// the enum will not contain an index_ty. In those cases the enum is a no-op.
    pub index_ty: Option<NativeType>,
    pub size: usize,
    pub alignment: usize,
}

impl Enum {
    pub fn layout(&self, module: &Module) -> anyhow::Result<EnumLayout> {
        if self.variants.is_empty() {
            return Ok(EnumLayout {
                alignment: 1,
                union_size: 0,
                union_offset: 0,
                size: 1,
                index_ty: None,
            });
        }

        let (union_size, alignment) =
            self.variants
                .iter()
                .fold(Ok((0, 1)), |res, member| -> anyhow::Result<_> {
                    let (size, align) = match res {
                        Err(err) => return Err(err),
                        Ok(tpl) => tpl,
                    };
                    Ok(if let Some(ty) = &member.ty {
                        let (ty_size, ty_align) = module.size_align(ty)?;
                        (ty_size.max(size), ty_align.max(align))
                    } else {
                        (size, align)
                    })
                })?;

        let index_ty = match self.variants.len() {
            0..2 => None,
            2..256 => Some(NativeType::U8),
            256..65536 => Some(NativeType::U16),
            _ => bail!("Enum must have less than 65,536 variants"),
        };

        let index_size = index_ty.as_ref().map(|ty| ty.size()).unwrap_or(0);
        let union_offset = pad_align(index_size, alignment);
        let alignment = index_size.max(alignment);
        let size = pad_align(union_size + union_offset, alignment);

        Ok(EnumLayout {
            union_size,
            union_offset,
            index_ty,
            size,
            alignment,
        })
    }
}

#[derive(PartialEq, Clone)]
pub struct PhiValue {
    pub block: u32,
    pub value: Value,
}

#[derive(Clone)]
pub struct PhiNode {
    pub ty: NativeType,
    pub variants: Vec<PhiValue>,
}

pub struct Block {
    /// Phi nodes all declared at the "beginning" of blocks for eazy analysis
    pub phi_nodes: Vec<PhiNode>,
    pub ops: Vec<Op>,
    pub terminator: TermOp,
}

pub struct Function {
    pub id: String,
    pub input: Vec<Ty>,
    pub output: Option<Ty>,
    pub blocks: Vec<Block>,
}

pub struct IRPipeline {
    pub id: String,
    pub input: Vec<Ty>,
    pub output: Option<Ty>,
    /// Function ids
    pub stages: Vec<i32>,
}

pub struct Module {
    pub id: String,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub functions: Vec<Function>,
    pub pipelines: Vec<IRPipeline>,
}

impl Module {
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            structs: Default::default(),
            enums: Default::default(),
            functions: Default::default(),
            pipelines: Default::default(),
        }
    }

    pub fn size_align(&self, ty: &Ty) -> anyhow::Result<(usize, usize)> {
        Ok(match ty {
            Ty::Native(ty) => (ty.size(), ty.alignment()),
            Ty::Struct(ty) => {
                let s = self
                    .get_struct(*ty)
                    .ok_or_else(|| anyhow!("couldn't find struct {}", ty))?
                    .layout(self)?;
                (s.size, s.alignment)
            }
            Ty::Enum(ty) => {
                let s = self
                    .get_enum(*ty)
                    .ok_or_else(|| anyhow!("couldn't find enum {}", ty))?
                    .layout(self)?;
                (s.size, s.alignment)
            }
        })
    }

    pub fn get_struct(&self, id: StructId) -> Option<&Struct> {
        self.structs.get(id as usize)
    }

    pub fn get_enum(&self, id: EnumId) -> Option<&Enum> {
        self.enums.get(id as usize)
    }

    pub fn get_function(&self, id: FnId) -> Option<&Function> {
        self.functions.get(id as usize)
    }

    pub fn get_pipeline(&self, id: PipeId) -> Option<&IRPipeline> {
        self.pipelines.get(id as usize)
    }

    // Recursively convert a type to a vec of every native type value.
    pub fn flatten(&self, ty: &Ty) -> Vec<NativeType> {
        let mut values = Vec::new();
        self.flatten_internal(ty, &mut values);
        values
    }

    fn flatten_internal(&self, ty: &Ty, values: &mut Vec<NativeType>) {
        match ty {
            Ty::Native(native_type) => values.push(native_type.clone()),
            Ty::Struct(id) => {
                let def = self.get_struct(*id).unwrap();
                for member in &def.members {
                    self.flatten_internal(&member.ty, values);
                }
            }
            Ty::Enum(id) => {
                let def = self.get_enum(*id).unwrap();
                if let Some(index_ty) = def.layout(self).unwrap().index_ty {
                    values.push(index_ty);
                }
                for v in &def.variants {
                    if let Some(ty) = &v.ty {
                        self.flatten_internal(ty, values);
                    }
                }
            }
        }
    }

    pub fn ty_string(&self, ty: &Ty) -> String {
        match ty {
            Ty::Native(nt) => match nt {
                NativeType::Ptr(is_mut, inner_ty) => format!(
                    "*{} {}",
                    if *is_mut { "mut" } else { "const" },
                    if let Some(inner_ty) = inner_ty {
                        &self.ty_string(inner_ty)
                    } else {
                        "()"
                    }
                ),
                nt => nt.to_string(),
            },
            Ty::Struct(id) => {
                let def = self.get_struct(*id).unwrap();
                def.id.clone().unwrap_or_else(|| {
                    format!(
                        "({})",
                        def.members
                            .iter()
                            .map(|m| self.ty_string(&m.ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                })
            }
            Ty::Enum(id) => {
                let def = self.get_enum(*id).unwrap();
                def.id.clone().unwrap_or_else(|| {
                    format!(
                        "enum {{{}}}",
                        def.variants
                            .iter()
                            .map(|m| format!(
                                "{}{}",
                                m.id,
                                if let Some(ty) = &m.ty {
                                    &format!(": {}", self.ty_string(ty))
                                } else {
                                    ""
                                }
                            ))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                })
            }
        }
    }
}

use std::fmt::{self, Display};
impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ref::Struct(s) => write!(f, "Struct({})", s),
            Ref::Enum(e) => write!(f, "Enum({})", e),
            Ref::Function(c) => write!(f, "Function({})", c),
            Ref::Pipeline(p) => write!(f, "Pipeline({})", p),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Native(native) => write!(f, "{}", native),
            Ty::Struct(id) => write!(f, "struct({})", id),
            Ty::Enum(id) => write!(f, "enum({})", id),
        }
    }
}
impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstValue::Bool(val) => write!(f, "{}", val),
            ConstValue::U8(val) => write!(f, "{}u8", val),
            ConstValue::I8(val) => write!(f, "{}i8", val),
            ConstValue::U16(val) => write!(f, "{}u16", val),
            ConstValue::I16(val) => write!(f, "{}i16", val),
            ConstValue::U32(val) => write!(f, "{}u32", val),
            ConstValue::I32(val) => write!(f, "{}i32", val),
            ConstValue::F32(val) => write!(f, "{}f32", val),
            ConstValue::U64(val) => write!(f, "{}u64", val),
            ConstValue::I64(val) => write!(f, "{}i64", val),
            ConstValue::F64(val) => write!(f, "{}f64", val),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::PhiArg { block, arg } => {
                write!(f, "$PhiArg(block: {}, arg: {})", block, arg)
            }
            Value::FnArg(arg) => write!(f, "$FnArg({})", arg),
            Value::VecFnArg(arg, index) => write!(f, "$VecFnArg({}, {})", arg, index),
            Value::BlockOp { block, op } => write!(f, "$BlockOp(block: {}, op: {})", block, op),
            Value::VecBlockOp { block, op, index } => write!(
                f,
                "$BlockOp(block: {}, op: {}, index: {})",
                block, op, index
            ),
            Value::Const(val) => write!(f, "$Const({})", val),
        }
    }
}
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::INeg => write!(f, "ineg"),
            UnaryOp::FNeg => write!(f, "fneg"),
            UnaryOp::Alloc => write!(f, "alloc"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::IAdd => write!(f, "iadd"),
            BinaryOp::FAdd => write!(f, "fadd"),
            BinaryOp::ISub => write!(f, "isub"),
            BinaryOp::FSub => write!(f, "fsub"),
            BinaryOp::IMul => write!(f, "imul"),
            BinaryOp::FMul => write!(f, "fmul"),
            BinaryOp::SDiv => write!(f, "sdiv"),
            BinaryOp::UDiv => write!(f, "udiv"),
            BinaryOp::FDiv => write!(f, "fdiv"),
            BinaryOp::SRem => write!(f, "srem"),
            BinaryOp::URem => write!(f, "urem"),
            BinaryOp::SCmp(cty) => write!(f, "s{}", cty),
            BinaryOp::UCmp(cty) => write!(f, "u{}", cty),
            BinaryOp::FCmp(cty) => write!(f, "f{}", cty),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::ShiftL => write!(f, "shl"),
            BinaryOp::IShiftR => write!(f, "ishr"),
            BinaryOp::UShiftR => write!(f, "ushr"),
        }
    }
}

impl fmt::Display for CmpTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CmpTy::Eq => write!(f, "eq"),
            CmpTy::Ne => write!(f, "ne"),
            CmpTy::Gt => write!(f, "gt"),
            CmpTy::Ge => write!(f, "ge"),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Load { ty, ptr } => write!(f, "(load {} {})", ty, ptr),
            Op::Store { src, ptr } => write!(f, "(store {} {})", src, ptr),
            Op::Unary { op, value } => write!(f, "({} {})", op, value),
            Op::Binary { op, left, right } => write!(f, "({} {} {})", op, left, right),
            Op::Call { func, input } => write!(
                f,
                "(call {} [{}])",
                func,
                input
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Op::CallIndirect {
                func_handle: func,
                input,
            } => write!(
                f,
                "(call_indirect {} [{}])",
                func,
                input
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        }
    }
}

impl Display for TermOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TermOp::Ret(Some(out_v)) => {
                write!(
                    f,
                    "(ret [{}])",
                    out_v
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            TermOp::Ret(None) => {
                write!(f, "ret")
            }
            TermOp::Jump { block } => write!(f, "(jump {})", block),
            TermOp::JumpIf { cond, then, else_ } => {
                write!(f, "(jump_if {} then {} else {})", cond, then, else_)
            }
            TermOp::JumpMap {
                cond,
                branches,
                default,
            } => {
                write!(
                    f,
                    "(jump_map {} default: {} branches: [{}])",
                    cond,
                    default,
                    branches
                        .iter()
                        .map(|(val, blk)| format!("({}, {})", val, blk))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl fmt::Display for FnArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FnArg::Value(value) => write!(f, "{}", value),
            FnArg::Obj(values) => write!(
                f,
                "Struct({})",
                values
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(func \"{}\" ", self.id)?;
        write!(f, "(")?;
        for (i, ty) in self.input.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, ") ")?;
        match &self.output {
            Some(ty) => write!(f, "{}", ty)?,
            None => write!(f, "nil")?,
        }
        for (i, block) in self.blocks.iter().enumerate() {
            write!(f, "\n   Block {}:", i)?;
            for (i, phi) in block.phi_nodes.iter().enumerate() {
                write!(f, "\n    ϕ{}({}): ", i, phi.ty)?;
                for phi_source in &phi.variants {
                    write!(f, "(src: {} val: {})", phi_source.block, phi_source.value)?;
                }
            }
            for (j, op) in block.ops.iter().enumerate() {
                write!(f, "\n      {:4} <- {}", j, op)?;
            }
            write!(f, "\n      Term: {}", block.terminator)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for IRPipeline {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(pipe \"{}\" ", self.id)?;
        write!(f, "(")?;
        for (i, ty) in self.input.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, ") ")?;
        match &self.output {
            Some(ty) => write!(f, "{}", ty)?,
            None => write!(f, "nil")?,
        }
        write!(f, " (stages")?;
        for stage in &self.stages {
            write!(f, " {}", stage)?;
        }
        write!(f, "))")
    }
}

impl fmt::Display for StructMember {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.id {
            Some(id) => write!(f, "(\"{}\" {})", id, self.ty),
            None => write!(f, "({})", self.ty),
        }
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let id = self.id.as_deref().unwrap_or("-anon");
        write!(f, "(struct \"{}\"", id)?;
        for member in &self.members {
            write!(f, " {}", member)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(module \"{}\"", self.id)?;
        for s in &self.structs {
            write!(f, "\n{}", s)?;
        }
        for func in &self.functions {
            write!(f, "\n{}", func)?;
        }
        for pipe in &self.pipelines {
            write!(f, "\n{}", pipe)?;
        }
        write!(f, ")")
    }
}

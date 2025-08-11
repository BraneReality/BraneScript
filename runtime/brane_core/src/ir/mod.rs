use anyhow::{anyhow, bail};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Ref {
    Struct(i32),
    Enum(i32),
    Function(i32),
    Pipeline(i32),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
}

impl NativeType {
    pub fn to_str(&self) -> &'static str {
        use NativeType::*;
        match self {
            Bool => "bool",
            U8 => "u8",
            I8 => "i8",
            U16 => "u16",
            I16 => "i16",
            U32 => "u32",
            I32 => "i32",
            F32 => "f32",
            U64 => "u64",
            I64 => "i64",
            F64 => "f64",
            U128 => "u128",
            I128 => "i128",
        }
    }

    pub fn from_str(text: &str) -> Result<NativeType, ()> {
        use NativeType::*;
        match text {
            "bool" => Ok(Bool),
            "u8" => Ok(U8),
            "i8" => Ok(I8),
            "u16" => Ok(U16),
            "i16" => Ok(I16),
            "u32" => Ok(U32),
            "i32" => Ok(I32),
            "f32" => Ok(F32),
            "u64" => Ok(U64),
            "i64" => Ok(I64),
            "f64" => Ok(F64),
            "u128" => Ok(U128),
            "i128" => Ok(I128),
            _ => Err(()),
        }
    }

    pub fn size(&self) -> usize {
        use NativeType::*;
        match self {
            Bool | I8 | U8 => 1,
            I16 | U16 => 2,
            I32 | U32 | F32 => 4,
            I64 | U64 | F64 => 8,
            U128 | I128 => 16,
        }
    }

    pub fn alignment(&self) -> usize {
        self.size()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Ty {
    Native(NativeType),
    Struct(i32),
    Enum(i32),
    /// (is mutable, inner ty)
    Ptr(bool, Option<Box<Ty>>),
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Value {
    PhiArg { block: u32, arg: u32 },
    FnArg(u32),
    BlockOp { block: u32, op: u32 },
    ConstI32(i32),
    ConstI64(i64),
    ConstF32(f32),
    ConstF64(f64),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Op {
    AllocA {
        ty: Ty,
    },
    Load {
        ty: NativeType,
        ptr: Value,
    },
    Store {
        src: Value,
        ptr: Value,
    },
    // Unary ops
    INeg {
        arg: Value,
    },
    FNeg {
        arg: Value,
    },
    // Binary ops
    IAdd {
        left: Value,
        right: Value,
    },
    FAdd {
        left: Value,
        right: Value,
    },
    ISub {
        left: Value,
        right: Value,
    },
    FSub {
        left: Value,
        right: Value,
    },
    IMul {
        left: Value,
        right: Value,
    },
    FMul {
        left: Value,
        right: Value,
    },
    SDiv {
        left: Value,
        right: Value,
    },
    UDiv {
        left: Value,
        right: Value,
    },
    FDiv {
        left: Value,
        right: Value,
    },
    URem {
        left: Value,
        right: Value,
    },
    SRem {
        left: Value,
        right: Value,
    },
    FRem {
        left: Value,
        right: Value,
    },
    CmpEq {
        left: Value,
        right: Value,
    },
    CmpNe {
        left: Value,
        right: Value,
    },
    CmpGt {
        left: Value,
        right: Value,
    },
    CmpGe {
        left: Value,
        right: Value,
    },
    And {
        left: Value,
        right: Value,
    },
    Or {
        left: Value,
        right: Value,
    },
    Xor {
        left: Value,
        right: Value,
    },
    ShiftL {
        left: Value,
        right: Value,
    },
    IShiftR {
        left: Value,
        right: Value,
    },
    UShiftR {
        left: Value,
        right: Value,
    },
    Call {
        func: i32,
        input: Vec<Value>,
        output: Value,
    },
    Jump {
        block: i32,
    },
    JumpIf {
        cond: Value,
        block: i32,
    },
    JumpTable {
        cond: Value,
        block: i32,
    },
    Ret(Option<(Ty, Value)>),
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
    pub index_offset: usize,
    pub index_ty: NativeType,
    pub size: usize,
    pub alignment: usize,
}

impl Enum {
    pub fn layout(&self, module: &Module) -> anyhow::Result<EnumLayout> {
        if self.variants.is_empty() {
            return Ok(EnumLayout {
                alignment: 1,
                union_size: 0,
                index_offset: 0,
                size: 1,
                index_ty: NativeType::U8,
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
            0..256 => NativeType::U8,
            256..65536 => NativeType::U16,
            _ => bail!("Enum must have less than 65,536 variants"),
        };

        let alignment = index_ty.alignment().max(alignment);
        let index_offset = pad_align(union_size, index_ty.alignment());
        let size = pad_align(index_offset + index_ty.size(), alignment);

        Ok(EnumLayout {
            union_size,
            index_offset,
            index_ty,
            size,
            alignment,
        })
    }
}

pub struct Block {
    /// Phi nodes all declared at the "beginning" of blocks for eazy analysis
    pub phi: Vec<Vec<Value>>,
    pub ops: Vec<Op>,
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
            Ty::Ptr(_, _) => (4, 4),
        })
    }

    pub fn get_struct(&self, id: i32) -> Option<&Struct> {
        self.structs.get(id as usize)
    }

    pub fn get_function(&self, id: i32) -> Option<&Function> {
        self.functions.get(id as usize)
    }

    pub fn get_pipeline(&self, id: i32) -> Option<&IRPipeline> {
        self.pipelines.get(id as usize)
    }

    pub fn get_enum(&self, id: i32) -> Option<&Enum> {
        self.enums.get(id as usize)
    }
}

use std::fmt;
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

impl fmt::Display for NativeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Native(native) => write!(f, "{}", native),
            Ty::Struct(id) => write!(f, "struct({})", id),
            Ty::Enum(id) => write!(f, "enum({})", id),
            Ty::Ptr(mutable, ty) => {
                write!(f, "{}", if *mutable { "ptr" } else { "mut_ptr" })?;
                if let Some(ty) = ty {
                    write!(f, "{}", ty)?;
                }
                Ok(())
            }
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
            Value::BlockOp { block, op } => write!(f, "$BlockOp(block: {}, op: {})", block, op),
            Value::ConstI32(val) => write!(f, "$ConstI32({})", val),
            Value::ConstI64(val) => write!(f, "$ConstI64({})", val),
            Value::ConstF32(val) => write!(f, "$ConstF32({})", val),
            Value::ConstF64(val) => write!(f, "$ConstF64({})", val),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::AllocA { ty } => write!(f, "(alloc {})", ty),
            Op::Load { ty, ptr } => write!(f, "(load {} {})", ty.to_str(), ptr),
            Op::Store { src, ptr } => write!(f, "(store {} {})", src, ptr),
            Op::INeg { arg } => write!(f, "(i_neg {})", arg),
            Op::FNeg { arg } => write!(f, "(f_neg {})", arg),
            Op::IAdd { left, right } => write!(f, "(i_add {} {})", left, right),
            Op::FAdd { left, right } => write!(f, "(s_add {} {})", left, right),
            Op::ISub { left, right } => write!(f, "(i_sub {} {})", left, right),
            Op::FSub { left, right } => write!(f, "(f_sub {} {})", left, right),
            Op::IMul { left, right } => write!(f, "(i_mul {} {})", left, right),
            Op::FMul { left, right } => write!(f, "(f_mul {} {})", left, right),
            Op::SDiv { left, right } => write!(f, "(s_div {} {})", left, right),
            Op::UDiv { left, right } => write!(f, "(u_div {} {})", left, right),
            Op::FDiv { left, right } => write!(f, "(f_div {} {})", left, right),
            Op::SRem { left, right } => write!(f, "(s_rem {} {})", left, right),
            Op::URem { left, right } => write!(f, "(u_rem {} {})", left, right),
            Op::FRem { left, right } => write!(f, "(f_rem {} {})", left, right),
            Op::CmpEq { left, right } => write!(f, "(eq {} {})", left, right),
            Op::CmpNe { left, right } => write!(f, "(ne {} {})", left, right),
            Op::CmpGt { left, right } => write!(f, "(gt {} {})", left, right),
            Op::CmpGe { left, right } => write!(f, "(ge {} {})", left, right),
            Op::And { left, right } => write!(f, "(and {} {})", left, right),
            Op::Or { left, right } => write!(f, "(or {} {})", left, right),
            Op::Xor { left, right } => write!(f, "(xor {} {})", left, right),
            Op::ShiftL { left, right } => write!(f, "(shl {} {})", left, right),
            Op::IShiftR { left, right } => write!(f, "(i_shr {} {})", left, right),
            Op::UShiftR { left, right } => write!(f, "(u_shr {} {})", left, right),
            Op::Call {
                func,
                input,
                output,
            } => write!(
                f,
                "(call {} [{}] {})",
                func,
                input
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                output
            ),
            Op::Ret(Some((out_t, out_v))) => {
                write!(f, "(ret {} {})", out_t, out_v)
            }
            Op::Ret(None) => {
                write!(f, "ret")
            }
            Op::Jump { block } => write!(f, "(jump {})", block),
            Op::JumpIf { cond, block } => write!(f, "(jump_if {} {})", cond, block),
            Op::JumpTable { cond, block } => write!(f, "(jump_table {} {})", cond, block),
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
            for (j, op) in block.ops.iter().enumerate() {
                write!(f, "\n      {:4} <- {}", j, op)?;
            }
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

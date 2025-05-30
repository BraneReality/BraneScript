use anyhow::anyhow;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct IRIDRef(pub u32);

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IRNativeType {
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
    Ptr,
    FnPtr,
}

impl IRNativeType {
    pub fn to_str(&self) -> &'static str {
        use IRNativeType::*;
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
            Ptr => "ptr",
            FnPtr => "fnPtr",
        }
    }

    pub fn from_str(text: &str) -> Result<IRNativeType, ()> {
        use IRNativeType::*;
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
            "ptr" => Ok(Ptr),
            "fnPtr" => Ok(FnPtr),
            _ => Err(()),
        }
    }

    pub fn size(&self) -> usize {
        use IRNativeType::*;
        match self {
            Bool | I8 | U8 => 1,
            I16 | U16 => 2,
            I32 | U32 | F32 | Ptr => 4,
            I64 | U64 | F64 => 8,
            U128 | I128 => 16,
            FnPtr => todo!("fn pointer not implemented"),
        }
    }

    pub fn alignment(&self) -> usize {
        self.size()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum IRType {
    Native(IRNativeType),
    Struct(IRIDRef),
}

impl IRIDRef {
    pub fn size_layout(&self, module: &IRModule) -> anyhow::Result<(usize, usize)> {
        let layout = module
            .get_struct(self)
            .ok_or(anyhow!("couldn't find struct {}", self))?
            .layout(module)?;
        Ok((layout.size, layout.alignment))
    }
}

impl IRType {
    pub fn size_layout(&self, module: &IRModule) -> anyhow::Result<(usize, usize)> {
        Ok(match self {
            IRType::Native(nt) => (nt.size(), nt.size()),
            IRType::Struct(idref) => idref.size_layout(module)?,
        })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct IRValue(pub u32);

#[derive(PartialEq, Clone, Debug)]
pub enum IROp {
    ArgValue {
        index: u32,
    },
    ConstI32 {
        value: i32,
    },
    ConstF32 {
        value: f32,
    },
    AllocA {
        r#type: IRType,
    },
    Load {
        r#type: IRNativeType,
        ptr: IRValue,
    },
    Store {
        src: IRValue,
        ptr: IRValue,
    },
    // Unary ops
    INeg {
        arg: IRValue,
    },
    FNeg {
        arg: IRValue,
    },
    // Binary ops
    IAdd {
        left: IRValue,
        right: IRValue,
    },
    FAdd {
        left: IRValue,
        right: IRValue,
    },
    ISub {
        left: IRValue,
        right: IRValue,
    },
    FSub {
        left: IRValue,
        right: IRValue,
    },
    IMul {
        left: IRValue,
        right: IRValue,
    },
    FMul {
        left: IRValue,
        right: IRValue,
    },
    SDiv {
        left: IRValue,
        right: IRValue,
    },
    UDiv {
        left: IRValue,
        right: IRValue,
    },
    FDiv {
        left: IRValue,
        right: IRValue,
    },
    URem {
        left: IRValue,
        right: IRValue,
    },
    SRem {
        left: IRValue,
        right: IRValue,
    },
    FRem {
        left: IRValue,
        right: IRValue,
    },
    CmpEq {
        left: IRValue,
        right: IRValue,
    },
    CmpNe {
        left: IRValue,
        right: IRValue,
    },
    CmpGt {
        left: IRValue,
        right: IRValue,
    },
    CmpGe {
        left: IRValue,
        right: IRValue,
    },
    And {
        left: IRValue,
        right: IRValue,
    },
    Or {
        left: IRValue,
        right: IRValue,
    },
    Xor {
        left: IRValue,
        right: IRValue,
    },
    ShiftL {
        left: IRValue,
        right: IRValue,
    },
    IShiftR {
        left: IRValue,
        right: IRValue,
    },
    UShiftR {
        left: IRValue,
        right: IRValue,
    },
    Call {
        func: IRIDRef,
        input: IRValue,
        output: IRValue,
    },
    Ret {
        args_t: IRIDRef,
        output: Option<IRValue>,
    },
    NextStage {
        args_t: IRIDRef,
        args: IRValue,
    },
}

#[derive(Clone)]
pub struct IRStructMember {
    pub id: Option<String>,
    pub r#type: IRType,
}

pub struct IRStruct {
    pub id: Option<String>,
    pub members: Vec<IRStructMember>,
    pub packed: bool,
}

pub struct IRStructLayout {
    // offsets of struct members in bytes
    pub byte_offsets: Vec<usize>,
    // total size of the struct
    pub size: usize,
    pub alignment: usize,
}

impl IRStruct {
    pub fn layout(&self, module: &IRModule) -> anyhow::Result<IRStructLayout> {
        let mut byte_offsets = Vec::with_capacity(self.members.len());
        let mut offset = 0;
        let mut max_alignment = 1;

        if self.members.is_empty() {
            return Ok(IRStructLayout {
                byte_offsets,
                size: 1,
                alignment: 1,
            });
        }

        for member in &self.members {
            let (size, align) = member.r#type.size_layout(module)?;

            if !self.packed {
                offset = (offset + align - 1) & !(align - 1); // Align offset
            }
            byte_offsets.push(offset);
            offset += size;
            if !self.packed {
                max_alignment = max_alignment.max(align);
            }
        }

        if !self.packed {
            offset = (offset + max_alignment - 1) & !(max_alignment - 1); // Align total size
        }

        assert_ne!(0, offset, "structs with members cannot be zero sized!");

        Ok(IRStructLayout {
            byte_offsets,
            size: offset,
            alignment: max_alignment,
        })
    }
}

pub struct IRFunction {
    pub id: String,
    pub input: IRIDRef,
    pub output: IRIDRef,
    pub operations: Vec<IROp>,
}

pub struct IRPipeline {
    pub id: String,
    pub input: IRIDRef,
    pub output: IRIDRef,
    pub stages: Vec<IRIDRef>,
}

pub struct IRModule {
    pub id: String,
    pub structs: Vec<IRStruct>,
    pub functions: Vec<IRFunction>,
    pub pipelines: Vec<IRPipeline>,
}

impl IRModule {
    pub fn get_struct(&self, id: &IRIDRef) -> Option<&IRStruct> {
        self.structs.get(id.0 as usize)
    }
}

use std::fmt;
impl fmt::Display for IRIDRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl fmt::Display for IRNativeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IRType::Native(native) => write!(f, "{}", native),
            IRType::Struct(id) => write!(f, "{}", id),
        }
    }
}

impl fmt::Display for IRValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "$({})", self.0)
    }
}

impl fmt::Display for IROp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IROp::ArgValue { index } => write!(f, "(arg {})", index),
            IROp::ConstI32 { value } => write!(f, "(const.i32 {})", value),
            IROp::ConstF32 { value } => write!(f, "(const.f32 {})", value),
            IROp::AllocA { r#type } => write!(f, "(alloc {})", r#type),
            IROp::Load { r#type, ptr } => write!(f, "(load {} {})", r#type.to_str(), ptr),
            IROp::Store { src, ptr } => write!(f, "(store {} {})", src, ptr),
            IROp::INeg { arg } => write!(f, "(i_neg {})", arg),
            IROp::FNeg { arg } => write!(f, "(f_neg {})", arg),
            IROp::IAdd { left, right } => write!(f, "(i_add {} {})", left, right),
            IROp::FAdd { left, right } => write!(f, "(s_add {} {})", left, right),
            IROp::ISub { left, right } => write!(f, "(i_sub {} {})", left, right),
            IROp::FSub { left, right } => write!(f, "(f_sub {} {})", left, right),
            IROp::IMul { left, right } => write!(f, "(i_mul {} {})", left, right),
            IROp::FMul { left, right } => write!(f, "(f_mul {} {})", left, right),
            IROp::SDiv { left, right } => write!(f, "(s_div {} {})", left, right),
            IROp::UDiv { left, right } => write!(f, "(u_div {} {})", left, right),
            IROp::FDiv { left, right } => write!(f, "(f_div {} {})", left, right),
            IROp::SRem { left, right } => write!(f, "(s_rem {} {})", left, right),
            IROp::URem { left, right } => write!(f, "(u_rem {} {})", left, right),
            IROp::FRem { left, right } => write!(f, "(f_rem {} {})", left, right),
            IROp::CmpEq { left, right } => write!(f, "(eq {} {})", left, right),
            IROp::CmpNe { left, right } => write!(f, "(ne {} {})", left, right),
            IROp::CmpGt { left, right } => write!(f, "(gt {} {})", left, right),
            IROp::CmpGe { left, right } => write!(f, "(ge {} {})", left, right),
            IROp::And { left, right } => write!(f, "(and {} {})", left, right),
            IROp::Or { left, right } => write!(f, "(or {} {})", left, right),
            IROp::Xor { left, right } => write!(f, "(xor {} {})", left, right),
            IROp::ShiftL { left, right } => write!(f, "(shl {} {})", left, right),
            IROp::IShiftR { left, right } => write!(f, "(i_shr {} {})", left, right),
            IROp::UShiftR { left, right } => write!(f, "(u_shr {} {})", left, right),
            IROp::Call {
                func,
                input,
                output,
            } => write!(f, "(call {} {} {})", func, input, output),
            IROp::Ret { args_t, output } => {
                if let Some(out_val) = &output {
                    write!(f, "(ret {} {})", args_t, out_val)
                } else {
                    write!(f, "(ret {})", args_t)
                }
            }
            IROp::NextStage { args_t, args } => {
                write!(f, "(stage.next {} {})", args_t, args)
            }
        }
    }
}

impl fmt::Display for IRFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(func \"{}\" {} {}", self.id, self.input, self.output)?;
        for (i, op) in self.operations.iter().enumerate() {
            write!(f, "\n   {:4} <- {}", i, op)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for IRPipeline {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(pipe \"{}\" {} {} (stages",
            self.id, self.input, self.output
        )?;
        for stage in &self.stages {
            write!(f, " {}", stage)?;
        }
        write!(f, "))")
    }
}

impl fmt::Display for IRStructMember {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.id {
            Some(id) => write!(f, "(\"{}\" {})", id, self.r#type),
            None => write!(f, "({})", self.r#type),
        }
    }
}

impl fmt::Display for IRStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let id = self.id.as_deref().unwrap_or("-anon");
        write!(f, "(struct \"{}\"", id)?;
        for member in &self.members {
            write!(f, " {}", member)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for IRModule {
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

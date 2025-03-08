pub enum IDRef {
    Name(String),
    Index(i32),
}

pub enum IRNativeType {
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

pub enum IRType {
    Native(IRNativeType),
    Struct(IDRef),
}

pub struct IRValue(u32);

pub enum IROp {
    ConstI32 {
        value: i32,
    },
    ConstU32 {
        value: u32,
    },
    ConstF32 {
        value: f32,
    },
    Load {
        ptr: IRValue,
    },
    Store {
        src: IRValue,
        ptr: IRValue,
    },
    Add {
        left: IRValue,
        right: IRValue,
    },
    Sub {
        left: IRValue,
        right: IRValue,
    },
    Mul {
        left: IRValue,
        right: IRValue,
    },
    Div {
        left: IRValue,
        right: IRValue,
    },
    Rem {
        left: IRValue,
        right: IRValue,
    },
    Eq {
        left: IRValue,
        right: IRValue,
    },
    Ne {
        left: IRValue,
        right: IRValue,
    },
    Gt {
        left: IRValue,
        right: IRValue,
    },
    Ge {
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
    Call {
        func: IDRef,
        input: IRValue,
        output: IRValue,
    },
    NextStage {
        args: IRValue,
        deps: Option<IRValue>,
    },
}

pub struct IRStructMember {
    pub id: Option<String>,
    pub r#type: IRType,
}

pub struct IRStruct {
    pub id: Option<String>,
    pub members: Vec<IRStructMember>,
}

pub struct IRFunction {
    pub id: String,
    pub input: IDRef,
    pub output: IDRef,
    pub operations: Vec<IROp>,
}

pub struct IRPipeline {
    pub id: String,
    pub input: IDRef,
    pub outpu: IDRef,
    pub stages: Vec<IDRef>,
}

pub struct IRModule {
    pub id: String,
    pub structs: Vec<IRStruct>,
    pub functions: Vec<IRFunction>,
    pub pipelines: Vec<IRPipeline>,
}

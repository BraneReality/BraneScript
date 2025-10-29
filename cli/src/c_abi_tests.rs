/// Complete ABI Test Suite with JIT Integration (Written by Claude because I am NOT spending a day
/// writing out every single edge case)
///
/// This file contains everything needed to test C ABI compatibility:
/// - All type definitions with repr(C)
/// - JIT integration helper
/// - Complete test suite
/// - Utility functions and macros

// ============================================================================
// CARGO TEST INTEGRATION
// ============================================================================

#[cfg(test)]
mod tests {
    use brane_core::BindingsCtx;
    use branec_source::SourceManager;
    use std::collections::HashMap;
    use std::mem;

    use anyhow::Result;
    use brane_backend_cranelift::cranelift_jit::JITModule;
    use brane_backend_cranelift::cranelift_module::FuncId;
    use brane_backend_cranelift::CraneliftJitBackend;
    use branec::CompileContext;

    // ============================================================================
    // TYPE DEFINITIONS - These must match your script types exactly
    // ============================================================================

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Struct4B {
        pub field: i32,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Struct8B {
        pub field: i64,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Struct16B {
        pub field1: i64,
        pub field2: u64,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Struct16BF {
        pub field1: f32,
        pub field2: f32,
        pub field3: f32,
        pub field4: f32,
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Struct24B {
        pub field1: i64,
        pub field2: i64,
        pub field3: i64,
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumEmpty2 {
        Variant1,
        Variant2,
    }

    #[allow(dead_code)]
    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumEmpty256 {
        V1,
        V2,
        V3,
        V4,
        V5,
        V6,
        V7,
        V8,
        V9,
        V10,
        V11,
        V12,
        V13,
        V14,
        V15,
        V16,
        V17,
        V18,
        V19,
        V20,
        V21,
        V22,
        V23,
        V24,
        V25,
        V26,
        V27,
        V28,
        V29,
        V30,
        V31,
        V32,
        V33,
        V34,
        V35,
        V36,
        V37,
        V38,
        V39,
        V40,
        V41,
        V42,
        V43,
        V44,
        V45,
        V46,
        V47,
        V48,
        V49,
        V50,
        V51,
        V52,
        V53,
        V54,
        V55,
        V56,
        V57,
        V58,
        V59,
        V60,
        V61,
        V62,
        V63,
        V64,
        V65,
        V66,
        V67,
        V68,
        V69,
        V70,
        V71,
        V72,
        V73,
        V74,
        V75,
        V76,
        V77,
        V78,
        V79,
        V80,
        V81,
        V82,
        V83,
        V84,
        V85,
        V86,
        V87,
        V88,
        V89,
        V90,
        V91,
        V92,
        V93,
        V94,
        V95,
        V96,
        V97,
        V98,
        V99,
        V100,
        V101,
        V102,
        V103,
        V104,
        V105,
        V106,
        V107,
        V108,
        V109,
        V110,
        V111,
        V112,
        V113,
        V114,
        V115,
        V116,
        V117,
        V118,
        V119,
        V120,
        V121,
        V122,
        V123,
        V124,
        V125,
        V126,
        V127,
        V128,
        V129,
        V130,
        V131,
        V132,
        V133,
        V134,
        V135,
        V136,
        V137,
        V138,
        V139,
        V140,
        V141,
        V142,
        V143,
        V144,
        V145,
        V146,
        V147,
        V148,
        V149,
        V150,
        V151,
        V152,
        V153,
        V154,
        V155,
        V156,
        V157,
        V158,
        V159,
        V160,
        V161,
        V162,
        V163,
        V164,
        V165,
        V166,
        V167,
        V168,
        V169,
        V170,
        V171,
        V172,
        V173,
        V174,
        V175,
        V176,
        V177,
        V178,
        V179,
        V180,
        V181,
        V182,
        V183,
        V184,
        V185,
        V186,
        V187,
        V188,
        V189,
        V190,
        V191,
        V192,
        V193,
        V194,
        V195,
        V196,
        V197,
        V198,
        V199,
        V200,
        V201,
        V202,
        V203,
        V204,
        V205,
        V206,
        V207,
        V208,
        V209,
        V210,
        V211,
        V212,
        V213,
        V214,
        V215,
        V216,
        V217,
        V218,
        V219,
        V220,
        V221,
        V222,
        V223,
        V224,
        V225,
        V226,
        V227,
        V228,
        V229,
        V230,
        V231,
        V232,
        V233,
        V234,
        V235,
        V236,
        V237,
        V238,
        V239,
        V240,
        V241,
        V242,
        V243,
        V244,
        V245,
        V246,
        V247,
        V248,
        V249,
        V250,
        V251,
        V252,
        V253,
        V254,
        V255,
        V256,
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumSingle4B {
        Variant(Struct4B),
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumSingle8B {
        Variant(Struct8B),
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumSingle16B {
        Variant(Struct16B),
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumSingle24B {
        Variant(Struct24B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMulti4B {
        Variant1(Struct4B),
        Variant2(Struct4B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMulti8B {
        Variant1(Struct8B),
        Variant2(Struct8B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMulti16B {
        Variant1(Struct16B),
        Variant2(Struct16B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMulti24B {
        Variant1(Struct24B),
        Variant2(Struct24B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMixed4B8B {
        Variant4B(Struct4B),
        Variant8B(Struct8B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMixed4B8B16B {
        Variant4B(Struct4B),
        Variant8B(Struct8B),
        Variant16B(Struct16B),
    }

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum EnumMixedAll {
        Variant4B(Struct4B),
        Variant8B(Struct8B),
        Variant16B(Struct16B),
        Variant24B(Struct24B),
    }

    // ============================================================================
    // JIT FUNCTION CONTAINER
    // ============================================================================

    /// Container for all JIT-compiled ABI test functions
    pub struct JitFunctions {
        pub abi_test_struct_4b: extern "C" fn(*mut BindingsCtx, Struct4B) -> Struct4B,
        pub abi_test_struct_8b: extern "C" fn(*mut BindingsCtx, Struct8B) -> Struct8B,
        pub abi_test_struct_16b: extern "C" fn(*mut BindingsCtx, Struct16B) -> Struct16B,
        pub abi_test_struct_16f: extern "C" fn(*mut BindingsCtx, Struct16BF) -> Struct16BF,
        pub abi_test_struct_24b: extern "C" fn(*mut BindingsCtx, Struct24B) -> Struct24B,
        pub abi_test_enum_empty_2: extern "C" fn(*mut BindingsCtx, EnumEmpty2) -> EnumEmpty2,
        pub abi_test_enum_empty_256: extern "C" fn(*mut BindingsCtx, EnumEmpty256) -> EnumEmpty256,
        pub abi_test_enum_single_4b: extern "C" fn(*mut BindingsCtx, EnumSingle4B) -> EnumSingle4B,
        pub abi_test_enum_single_8b: extern "C" fn(*mut BindingsCtx, EnumSingle8B) -> EnumSingle8B,
        pub abi_test_enum_single_16b:
            extern "C" fn(*mut BindingsCtx, EnumSingle16B) -> EnumSingle16B,
        pub abi_test_enum_single_24b:
            extern "C" fn(*mut BindingsCtx, EnumSingle24B) -> EnumSingle24B,
        pub abi_test_enum_multi_4b: extern "C" fn(*mut BindingsCtx, EnumMulti4B) -> EnumMulti4B,
        pub abi_test_enum_multi_8b: extern "C" fn(*mut BindingsCtx, EnumMulti8B) -> EnumMulti8B,
        pub abi_test_enum_multi_16b: extern "C" fn(*mut BindingsCtx, EnumMulti16B) -> EnumMulti16B,
        pub abi_test_enum_multi_24b: extern "C" fn(*mut BindingsCtx, EnumMulti24B) -> EnumMulti24B,
        pub abi_test_enum_mixed_4b_8b:
            extern "C" fn(*mut BindingsCtx, EnumMixed4B8B) -> EnumMixed4B8B,
        pub abi_test_enum_mixed_4b_8b_16b:
            extern "C" fn(*mut BindingsCtx, EnumMixed4B8B16B) -> EnumMixed4B8B16B,
        pub abi_test_enum_mixed_all: extern "C" fn(*mut BindingsCtx, EnumMixedAll) -> EnumMixedAll,
        pub abi_test_multi_params:
            extern "C" fn(*mut BindingsCtx, Struct4B, Struct8B, Struct16B, Struct24B) -> Struct24B,
        pub abi_test_exaust_general_6:
            extern "C" fn(*mut BindingsCtx, i32, f32, i32, i32, i32, Struct16B) -> Struct16B,
        pub abi_test_exaust_general_8: extern "C" fn(
            *mut BindingsCtx,
            i32,
            f32,
            i32,
            i32,
            i32,
            i32,
            i32,
            Struct16B,
        ) -> Struct16B,
        pub abi_test_exaust_vector_8: extern "C" fn(
            *mut BindingsCtx,
            f32,
            i32,
            f32,
            f32,
            f32,
            f32,
            f32,
            f32,
            Struct16BF,
        ) -> Struct16BF,
    }

    // ============================================================================
    // JIT INTEGRATION HELPER
    // ============================================================================

    /// Helper to load and manage JIT-compiled test functions
    pub struct AbiTestHarness {
        functions: JitFunctions,
    }

    impl AbiTestHarness {
        /// Create harness from your JIT backend
        ///
        /// # Example with your existing code structure
        /// ```ignore
        /// use brane_core::compile::{CompileContext, SourceManager};
        /// use brane_core::uri::Uri;
        /// use branec_emitter::ConsoleEmitter;
        ///
        /// let mut ctx = CompileContext {
        ///     emitter: ConsoleEmitter::new(),
        ///     sources: SourceManager::new(),
        ///     loaded_modules: Default::default(),
        /// };
        ///
        /// let module = ctx.emit_module(
        ///     &Uri::File("../tests/abi_test_script.bscript".into()),
        ///     vec!["abi_test".into()],
        /// )?;
        ///
        /// let mut jit = CraneliftJitBackend::default();
        /// let (fn_map, jit_module) = jit.jit(module)?;
        ///
        /// let harness = AbiTestHarness::from_jit_module(jit_module, fn_map)?;
        /// ```
        pub fn from_jit_module(module: JITModule, fn_map: HashMap<String, FuncId>) -> Result<Self> {
            macro_rules! get_fn {
                ($name:literal, $type:ty) => {{
                    let func_id = fn_map
                        .get($name)
                        .ok_or_else(|| anyhow::anyhow!("Function '{}' not found", $name))?;
                    println!("found {} as {}", $name, func_id);
                    let ptr = module.get_finalized_function(*func_id);
                    unsafe { std::mem::transmute::<_, $type>(ptr) }
                }};
            }

            let functions = JitFunctions {
                abi_test_struct_4b: get_fn!(
                    "abi_test_struct_4b",
                    extern "C" fn(*mut BindingsCtx, Struct4B) -> Struct4B
                ),
                abi_test_struct_8b: get_fn!(
                    "abi_test_struct_8b",
                    extern "C" fn(*mut BindingsCtx, Struct8B) -> Struct8B
                ),
                abi_test_struct_16b: get_fn!(
                    "abi_test_struct_16b",
                    extern "C" fn(*mut BindingsCtx, Struct16B) -> Struct16B
                ),
                abi_test_struct_16f: get_fn!(
                    "abi_test_struct_16f",
                    extern "C" fn(*mut BindingsCtx, Struct16BF) -> Struct16BF
                ),
                abi_test_struct_24b: get_fn!(
                    "abi_test_struct_24b",
                    extern "C" fn(*mut BindingsCtx, Struct24B) -> Struct24B
                ),
                abi_test_enum_empty_2: get_fn!(
                    "abi_test_enum_empty_2",
                    extern "C" fn(*mut BindingsCtx, EnumEmpty2) -> EnumEmpty2
                ),
                abi_test_enum_empty_256: get_fn!(
                    "abi_test_enum_empty_256",
                    extern "C" fn(*mut BindingsCtx, EnumEmpty256) -> EnumEmpty256
                ),
                abi_test_enum_single_4b: get_fn!(
                    "abi_test_enum_single_4b",
                    extern "C" fn(*mut BindingsCtx, EnumSingle4B) -> EnumSingle4B
                ),
                abi_test_enum_single_8b: get_fn!(
                    "abi_test_enum_single_8b",
                    extern "C" fn(*mut BindingsCtx, EnumSingle8B) -> EnumSingle8B
                ),
                abi_test_enum_single_16b: get_fn!(
                    "abi_test_enum_single_16b",
                    extern "C" fn(*mut BindingsCtx, EnumSingle16B) -> EnumSingle16B
                ),
                abi_test_enum_single_24b: get_fn!(
                    "abi_test_enum_single_24b",
                    extern "C" fn(*mut BindingsCtx, EnumSingle24B) -> EnumSingle24B
                ),
                abi_test_enum_multi_4b: get_fn!(
                    "abi_test_enum_multi_4b",
                    extern "C" fn(*mut BindingsCtx, EnumMulti4B) -> EnumMulti4B
                ),
                abi_test_enum_multi_8b: get_fn!(
                    "abi_test_enum_multi_8b",
                    extern "C" fn(*mut BindingsCtx, EnumMulti8B) -> EnumMulti8B
                ),
                abi_test_enum_multi_16b: get_fn!(
                    "abi_test_enum_multi_16b",
                    extern "C" fn(*mut BindingsCtx, EnumMulti16B) -> EnumMulti16B
                ),
                abi_test_enum_multi_24b: get_fn!(
                    "abi_test_enum_multi_24b",
                    extern "C" fn(*mut BindingsCtx, EnumMulti24B) -> EnumMulti24B
                ),
                abi_test_enum_mixed_4b_8b: get_fn!(
                    "abi_test_enum_mixed_4b_8b",
                    extern "C" fn(*mut BindingsCtx, EnumMixed4B8B) -> EnumMixed4B8B
                ),
                abi_test_enum_mixed_4b_8b_16b: get_fn!(
                    "abi_test_enum_mixed_4b_8b_16b",
                    extern "C" fn(*mut BindingsCtx, EnumMixed4B8B16B) -> EnumMixed4B8B16B
                ),
                abi_test_enum_mixed_all: get_fn!(
                    "abi_test_enum_mixed_all",
                    extern "C" fn(*mut BindingsCtx, EnumMixedAll) -> EnumMixedAll
                ),
                abi_test_multi_params: get_fn!(
                    "abi_test_multi_params",
                    extern "C" fn(
                        *mut BindingsCtx,
                        Struct4B,
                        Struct8B,
                        Struct16B,
                        Struct24B,
                    ) -> Struct24B
                ),
                abi_test_exaust_general_6: get_fn!(
                    "abi_test_exaust_general_6",
                    extern "C" fn(
                        *mut BindingsCtx,
                        i32,
                        f32,
                        i32,
                        i32,
                        i32,
                        Struct16B,
                    ) -> Struct16B
                ),
                abi_test_exaust_general_8: get_fn!(
                    "abi_test_exaust_general_8",
                    extern "C" fn(
                        *mut BindingsCtx,
                        i32,
                        f32,
                        i32,
                        i32,
                        i32,
                        i32,
                        i32,
                        Struct16B,
                    ) -> Struct16B
                ),
                abi_test_exaust_vector_8: get_fn!(
                    "abi_test_exaust_vector_8",
                    extern "C" fn(
                        *mut BindingsCtx,
                        f32,
                        i32,
                        f32,
                        f32,
                        f32,
                        f32,
                        f32,
                        f32,
                        Struct16BF,
                    ) -> Struct16BF
                ),
            };

            Ok(Self { functions })
        }

        /// Compile script and create harness in one step
        ///
        /// This is a convenience method that handles the entire compilation pipeline.
        /// Adjust the implementation to match your actual API.
        pub fn from_script_file() -> Result<Self> {
            let mut ctx = CompileContext {
                emitter: branec_emitter::ConsoleEmitter::new(),
                sources: SourceManager::new(),
                loaded_modules: Default::default(),
            };

            let test_source = ctx
                .sources
                .add_custom(include_str!("../../tests/c_abi.bscript").to_string())?;

            let module = ctx.emit_module(&test_source, vec!["test_mod".into()])?;

            println!("Emitted module:\n{}", module);
            println!("Jitting...");
            let mut jit = CraneliftJitBackend::default();
            let (fn_map, module) = jit.jit(module)?;
            Self::from_jit_module(module, fn_map)
        }

        /// Get reference to the loaded functions
        pub fn functions(&self) -> &JitFunctions {
            &self.functions
        }
    }

    // ============================================================================
    // INDIVIDUAL TEST IMPLEMENTATIONS
    // ============================================================================

    // Struct tests
    fn test_struct_4b(funcs: &JitFunctions) -> Result<()> {
        let input = Struct4B { field: 42 };
        let output = (funcs.abi_test_struct_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_8b(funcs: &JitFunctions) -> Result<()> {
        let input = Struct8B {
            field: 0x0123456789ABCDEF,
        };
        let output = (funcs.abi_test_struct_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_16b(funcs: &JitFunctions) -> Result<()> {
        let input = Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        };
        let output = (funcs.abi_test_struct_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_16bf(funcs: &JitFunctions) -> Result<()> {
        let input = Struct16BF {
            field1: 1.5,
            field2: -2.75,
            field3: 3.125,
            field4: -4.5,
        };
        let output = (funcs.abi_test_struct_16f)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_24b(funcs: &JitFunctions) -> Result<()> {
        let input = Struct24B {
            field1: 0x0123456789ABCDEF,
            field2: -1 as i64,
            field3: 0x1111111111111111,
        };
        let output = (funcs.abi_test_struct_24b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    // Empty enum tests
    fn test_enum_empty_2_variant1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumEmpty2::Variant1;
        let output = (funcs.abi_test_enum_empty_2)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_empty_2_variant2(funcs: &JitFunctions) -> Result<()> {
        let input = EnumEmpty2::Variant2;
        let output = (funcs.abi_test_enum_empty_2)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    // Empty enum with 256 variants tests
    fn test_enum_empty_256_v1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumEmpty256::V1;
        let output = (funcs.abi_test_enum_empty_256)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_empty_256_v128(funcs: &JitFunctions) -> Result<()> {
        let input = EnumEmpty256::V128;
        let output = (funcs.abi_test_enum_empty_256)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_empty_256_v256(funcs: &JitFunctions) -> Result<()> {
        let input = EnumEmpty256::V256;
        let output = (funcs.abi_test_enum_empty_256)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    // Single variant enum tests
    fn test_enum_single_4b(funcs: &JitFunctions) -> Result<()> {
        let input = EnumSingle4B::Variant(Struct4B { field: 42 });
        let output = (funcs.abi_test_enum_single_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_single_8b(funcs: &JitFunctions) -> Result<()> {
        let input = EnumSingle8B::Variant(Struct8B {
            field: 0x0123456789ABCDEF,
        });
        let output = (funcs.abi_test_enum_single_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_single_16b(funcs: &JitFunctions) -> Result<()> {
        let input = EnumSingle16B::Variant(Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        });
        let output = (funcs.abi_test_enum_single_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_enum_single_24b(funcs: &JitFunctions) -> Result<()> {
        let input = EnumSingle24B::Variant(Struct24B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210u64 as i64,
            field3: 0x1111111111111111,
        });
        let output = (funcs.abi_test_enum_single_24b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    // Multi variant same-size enum tests
    fn test_enum_multi_4b_v1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti4B::Variant1(Struct4B { field: 42 });
        let output = (funcs.abi_test_enum_multi_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti4B::Variant1(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_4b_v2(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti4B::Variant2(Struct4B { field: 99 });
        let output = (funcs.abi_test_enum_multi_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti4B::Variant2(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_8b_v1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti8B::Variant1(Struct8B {
            field: 0x0123456789ABCDEF,
        });
        let output = (funcs.abi_test_enum_multi_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti8B::Variant1(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_8b_v2(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti8B::Variant2(Struct8B {
            field: 0xFEDCBA9876543210u64 as i64,
        });
        let output = (funcs.abi_test_enum_multi_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti8B::Variant2(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_16b_v1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti16B::Variant1(Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        });
        let output = (funcs.abi_test_enum_multi_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti16B::Variant1(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_16b_v2(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti16B::Variant2(Struct16B {
            field1: 0x1111111111111111,
            field2: 0x2222222222222222,
        });
        let output = (funcs.abi_test_enum_multi_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti16B::Variant2(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_24b_v1(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti24B::Variant1(Struct24B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210u64 as i64,
            field3: 0x1111111111111111,
        });
        let output = (funcs.abi_test_enum_multi_24b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti24B::Variant1(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_multi_24b_v2(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMulti24B::Variant2(Struct24B {
            field1: 0x2222222222222222,
            field2: 0x3333333333333333,
            field3: 0x4444444444444444,
        });
        let output = (funcs.abi_test_enum_multi_24b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMulti24B::Variant2(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    // Mixed size enum tests
    fn test_enum_mixed_4b_8b_v4(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixed4B8B::Variant4B(Struct4B { field: 42 });
        let output = (funcs.abi_test_enum_mixed_4b_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixed4B8B::Variant4B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_4b_8b_v8(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixed4B8B::Variant8B(Struct8B {
            field: 0x0123456789ABCDEF,
        });
        let output = (funcs.abi_test_enum_mixed_4b_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixed4B8B::Variant8B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_4b_8b_16b_v4(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixed4B8B16B::Variant4B(Struct4B { field: 42 });
        let output = (funcs.abi_test_enum_mixed_4b_8b_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixed4B8B16B::Variant4B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_4b_8b_16b_v8(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixed4B8B16B::Variant8B(Struct8B {
            field: 0x0123456789ABCDEF,
        });
        let output = (funcs.abi_test_enum_mixed_4b_8b_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixed4B8B16B::Variant8B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_4b_8b_16b_v16(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixed4B8B16B::Variant16B(Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        });
        let output = (funcs.abi_test_enum_mixed_4b_8b_16b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixed4B8B16B::Variant16B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_all_v4(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixedAll::Variant4B(Struct4B { field: 1 });
        let output = (funcs.abi_test_enum_mixed_all)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixedAll::Variant4B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_all_v8(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixedAll::Variant8B(Struct8B {
            field: 0x0123456789ABCDEF,
        });
        let output = (funcs.abi_test_enum_mixed_all)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixedAll::Variant8B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_all_v16(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixedAll::Variant16B(Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        });
        let output = (funcs.abi_test_enum_mixed_all)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixedAll::Variant16B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    fn test_enum_mixed_all_v24(funcs: &JitFunctions) -> Result<()> {
        let input = EnumMixedAll::Variant24B(Struct24B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210u64 as i64,
            field3: 0x1111111111111111,
        });
        let output = (funcs.abi_test_enum_mixed_all)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        if !matches!(output, EnumMixedAll::Variant24B(_)) {
            anyhow::bail!("Discriminant changed to wrong variant");
        }
        Ok(())
    }

    // Multi parameter test
    fn test_multi_params(funcs: &JitFunctions) -> Result<()> {
        let s4 = Struct4B { field: 42 };
        let s8 = Struct8B {
            field: 0x0123456789ABCDEF,
        };
        let s16 = Struct16B {
            field1: 0xAAAAAAAAAAAAAAAAu64 as i64,
            field2: 0xBBBBBBBBBBBBBBBB,
        };
        let s24 = Struct24B {
            field1: 0x1111111111111111,
            field2: 0x2222222222222222,
            field3: 0x3333333333333333,
        };
        let output = (funcs.abi_test_multi_params)(std::ptr::null_mut(), s4, s8, s16, s24);
        if output != s24 {
            anyhow::bail!("Expected {:?}, got {:?}", s24, output);
        }
        Ok(())
    }

    // Exhaustion/general calling convention test
    fn test_exaust_general_6(funcs: &JitFunctions) -> Result<()> {
        let value = Struct16B {
            field1: 0x1122334455667788,
            field2: 0x99AABBCCDDEEFF00,
        };
        let output =
            (funcs.abi_test_exaust_general_6)(std::ptr::null_mut(), 1, 2.5, -3, 4, 5, value);
        if output != value {
            anyhow::bail!("Expected {:?}, got {:?}", value, output);
        }
        Ok(())
    }

    fn test_exaust_general_8(funcs: &JitFunctions) -> Result<()> {
        let value = Struct16B {
            field1: 0x1122334455667788,
            field2: 0x99AABBCCDDEEFF00,
        };
        let output =
            (funcs.abi_test_exaust_general_8)(std::ptr::null_mut(), 1, 2.5, -3, 4, 5, 6, 7, value);
        if output != value {
            anyhow::bail!("Expected {:?}, got {:?}", value, output);
        }
        Ok(())
    }

    fn test_exaust_vector_8(funcs: &JitFunctions) -> Result<()> {
        let value = Struct16BF {
            field1: 10.25,
            field2: -0.5,
            field3: 1234.5,
            field4: -1000.0,
        };
        let output = (funcs.abi_test_exaust_vector_8)(
            std::ptr::null_mut(),
            1.0f32,
            2i32,
            -3.5f32,
            4.25f32,
            2.54325,
            8.15124,
            3.14523,
            -3.1415,
            value,
        );
        if output != value {
            anyhow::bail!("Expected {:?}, got {:?}", value, output);
        }
        Ok(())
    }

    // Edge case tests
    fn test_struct_4b_zero(funcs: &JitFunctions) -> Result<()> {
        let input = Struct4B { field: 0 };
        let output = (funcs.abi_test_struct_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_4b_negative(funcs: &JitFunctions) -> Result<()> {
        let input = Struct4B { field: -42 };
        let output = (funcs.abi_test_struct_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_4b_max(funcs: &JitFunctions) -> Result<()> {
        let input = Struct4B { field: i32::MAX };
        let output = (funcs.abi_test_struct_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_4b_min(funcs: &JitFunctions) -> Result<()> {
        let input = Struct4B { field: i32::MIN };
        let output = (funcs.abi_test_struct_4b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_8b_max(funcs: &JitFunctions) -> Result<()> {
        let input = Struct8B { field: i64::MAX };
        let output = (funcs.abi_test_struct_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    fn test_struct_8b_min(funcs: &JitFunctions) -> Result<()> {
        let input = Struct8B { field: i64::MIN };
        let output = (funcs.abi_test_struct_8b)(std::ptr::null_mut(), input);
        if input != output {
            anyhow::bail!("Expected {:?}, got {:?}", input, output);
        }
        Ok(())
    }

    // Size verification tests - these don't need JIT
    #[test]
    fn test_struct_sizes() {
        assert_eq!(mem::size_of::<Struct4B>(), 4, "Struct4B should be 4 bytes");
        assert_eq!(mem::size_of::<Struct8B>(), 8, "Struct8B should be 8 bytes");
        assert_eq!(
            mem::size_of::<Struct16B>(),
            16,
            "Struct16B should be 16 bytes"
        );
        assert_eq!(
            mem::size_of::<Struct24B>(),
            24,
            "Struct24B should be 24 bytes"
        );
    }

    #[test]
    fn test_struct_alignment() {
        assert_eq!(
            mem::align_of::<Struct4B>(),
            4,
            "Struct4B should be 4-byte aligned"
        );
        assert_eq!(
            mem::align_of::<Struct8B>(),
            8,
            "Struct8B should be 8-byte aligned"
        );
        assert_eq!(
            mem::align_of::<Struct16B>(),
            8,
            "Struct16B should be 8-byte aligned"
        );
        assert_eq!(
            mem::align_of::<Struct24B>(),
            8,
            "Struct24B should be 8-byte aligned"
        );
    }

    #[test]
    fn test_enum_sizes() {
        // Empty enum should be just discriminant
        assert_eq!(
            mem::size_of::<EnumEmpty2>(),
            1,
            "EnumEmpty2 should be 1 byte"
        );
    }

    #[test]
    fn test_enum_alignment() {
        assert!(mem::align_of::<EnumEmpty2>() >= 1);
        assert!(mem::align_of::<EnumSingle4B>() >= 4);
        assert!(mem::align_of::<EnumSingle8B>() >= 8);
        assert!(mem::align_of::<EnumSingle16B>() >= 8);
        assert!(mem::align_of::<EnumSingle24B>() >= 8);
    }

    static TEST_HARNESS: std::sync::LazyLock<AbiTestHarness> = std::sync::LazyLock::new(|| {
        AbiTestHarness::from_script_file().expect("Unable to create test harness")
    });

    // Helper to load harness for tests
    fn get_test_harness() -> &'static AbiTestHarness {
        &TEST_HARNESS
    }

    #[test]
    fn test_struct_4b_jit() -> Result<()> {
        let harness = get_test_harness();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_4b(harness.functions()))? }
    }
    #[test]
    fn test_struct_8b_jit() -> Result<()> {
        let harness = get_test_harness();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_8b(harness.functions()))? }
    }

    #[test]
    fn test_struct_16b_jit() -> Result<()> {
        let harness = get_test_harness();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_16b(harness.functions()))? }
    }

    #[test]
    fn test_struct_16bf_jit() -> Result<()> {
        let harness = get_test_harness();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_16bf(harness.functions()))? }
    }

    #[test]
    fn test_struct_24b_jit() -> Result<()> {
        let harness = get_test_harness();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_24b(harness.functions()))? }
    }
    #[test]
    fn test_enum_empty_2_variant1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_empty_2_variant1(funcs))? }
    }

    #[test]
    fn test_enum_empty_2_variant2_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_empty_2_variant2(funcs))? }
    }

    #[test]
    fn test_enum_empty_256_v1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_empty_256_v1(funcs))? }
    }

    #[test]
    fn test_enum_empty_256_v128_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_empty_256_v128(funcs))? }
    }

    #[test]
    fn test_enum_empty_256_v256_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_empty_256_v256(funcs))? }
    }

    #[test]
    fn test_enum_single_4b_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_single_4b(funcs))? }
    }

    #[test]
    fn test_enum_single_8b_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_single_8b(funcs))? }
    }

    #[test]
    fn test_enum_single_16b_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_single_16b(funcs))? }
    }

    #[test]
    fn test_enum_single_24b_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_single_24b(funcs))? }
    }

    #[test]
    fn test_enum_multi_4b_v1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_4b_v1(funcs))? }
    }

    #[test]
    fn test_enum_multi_4b_v2_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_4b_v2(funcs))? }
    }

    #[test]
    fn test_enum_multi_8b_v1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_8b_v1(funcs))? }
    }

    #[test]
    fn test_enum_multi_8b_v2_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_8b_v2(funcs))? }
    }

    #[test]
    fn test_enum_multi_16b_v1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_16b_v1(funcs))? }
    }

    #[test]
    fn test_enum_multi_16b_v2_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_16b_v2(funcs))? }
    }

    #[test]
    fn test_enum_multi_24b_v1_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_24b_v1(funcs))? }
    }

    #[test]
    fn test_enum_multi_24b_v2_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_multi_24b_v2(funcs))? }
    }

    #[test]
    fn test_enum_mixed_4b_8b_v4_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_4b_8b_v4(funcs))? }
    }

    #[test]
    fn test_enum_mixed_4b_8b_v8_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_4b_8b_v8(funcs))? }
    }

    #[test]
    fn test_enum_mixed_4b_8b_16b_v4_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_4b_8b_16b_v4(funcs))? }
    }

    #[test]
    fn test_enum_mixed_4b_8b_16b_v8_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_4b_8b_16b_v8(funcs))? }
    }

    #[test]
    fn test_enum_mixed_4b_8b_16b_v16_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_4b_8b_16b_v16(funcs))? }
    }

    #[test]
    fn test_enum_mixed_all_v4_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_all_v4(funcs))? }
    }

    #[test]
    fn test_enum_mixed_all_v8_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_all_v8(funcs))? }
    }

    #[test]
    fn test_enum_mixed_all_v16_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_all_v16(funcs))? }
    }

    #[test]
    fn test_enum_mixed_all_v24_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_enum_mixed_all_v24(funcs))? }
    }

    #[test]
    fn test_struct_4b_zero_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_4b_zero(funcs))? }
    }

    #[test]
    fn test_struct_4b_negative_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_4b_negative(funcs))? }
    }

    #[test]
    fn test_struct_4b_max_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_4b_max(funcs))? }
    }

    #[test]
    fn test_struct_4b_min_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_4b_min(funcs))? }
    }

    #[test]
    fn test_struct_8b_max_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_8b_max(funcs))? }
    }

    #[test]
    fn test_struct_8b_min_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_struct_8b_min(funcs))? }
    }

    #[test]
    fn test_multi_params_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_multi_params(funcs))? }
    }

    #[test]
    fn test_exaust_general_6_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_exaust_general_6(funcs))? }
    }

    #[test]
    fn test_exaust_general_8_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_exaust_general_8(funcs))? }
    }

    #[test]
    fn test_exaust_vector_8_jit() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();
        unsafe { brane_core::sandbox::try_run(&mut || test_exaust_vector_8(funcs))? }
    }

    #[test]
    fn test_stress_repeated_calls() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();

        let input = Struct16B {
            field1: 0x0123456789ABCDEF,
            field2: 0xFEDCBA9876543210,
        };
        unsafe {
            brane_core::sandbox::try_run(&mut || {
                for i in 0..1000 {
                    let output = (funcs.abi_test_struct_16b)(std::ptr::null_mut(), input);
                    if input != output {
                        anyhow::bail!("Call {} failed: expected {:?}, got {:?}", i, input, output);
                    }
                }
                Ok(())
            })??;
        }

        Ok(())
    }

    #[test]
    fn test_stress_alternating_variants() -> Result<()> {
        let harness = get_test_harness();
        let funcs = harness.functions();

        unsafe {
            brane_core::sandbox::try_run(&mut || {
                for _ in 0..1000 {
                    let v1 = EnumMulti8B::Variant1(Struct8B {
                        field: 0xAAAAAAAAAAAAAAAAu64 as i64,
                    });
                    let out1 = (funcs.abi_test_enum_multi_8b)(std::ptr::null_mut(), v1);
                    if v1 != out1 {
                        anyhow::bail!("Variant1 failed");
                    }

                    let v2 = EnumMulti8B::Variant2(Struct8B {
                        field: 0xBBBBBBBBBBBBBBBBu64 as i64,
                    });
                    let out2 = (funcs.abi_test_enum_multi_8b)(std::ptr::null_mut(), v2);
                    if v2 != out2 {
                        anyhow::bail!("Variant2 failed");
                    }
                }
                Ok(())
            })??;
        }

        Ok(())
    }
}

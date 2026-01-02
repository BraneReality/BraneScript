use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{
    ir,
    memory::{FnTable, Page, PageTable, Store},
};

pub trait LoadedModule {
    fn get_fn(&self, name: &str) -> Option<*const u8>;
    fn get_function_names(&self) -> impl Iterator<Item = &str>;
}

pub trait JitBackend: Default {
    type ModuleTy: LoadedModule;

    fn load(&self, module: ir::Module) -> anyhow::Result<ModuleId>;

    fn get_module(&self, module: ModuleId) -> Option<Arc<Self::ModuleTy>>;
}

pub struct Runtime<B: JitBackend> {
    backend: B,
    store: Store,
    name_to_module: RwLock<HashMap<String, ModuleId>>,
}

pub type ModuleId = usize;

impl<B: JitBackend> Runtime<B> {
    pub fn new() -> Self {
        Self {
            backend: B::default(),
            store: Store::new(),
            name_to_module: Default::default(),
        }
    }

    /// Initializes the job runner and starts executing queued jobs. Non blocking.
    pub fn start_workers(n: Option<usize>) {
        todo!();
    }

    pub fn load_module(&mut self, module: ir::Module) -> anyhow::Result<ModuleId> {
        let name = module.id.clone();
        let id = self.backend.load(module)?;
        self.name_to_module.write().unwrap().insert(name, id);
        Ok(id)
    }

    pub fn get_module(&mut self, module: ModuleId) -> Option<Arc<B::ModuleTy>> {
        self.backend.get_module(module)
    }

    pub fn get_module_by_name(&mut self, name: impl AsRef<str>) -> Option<Arc<B::ModuleTy>> {
        let name = name.as_ref();

        let mod_id = self.name_to_module.read().unwrap().get(name).cloned()?;
        self.backend.get_module(mod_id)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BindingError {
    NoFreeBindings,
}

#[repr(C)]
pub struct ExecutionCtx {
    bindings: Box<PageTable>,
    /// This needs to be managed by a runtime manager that either appends only, or pulls from a
    /// list of free'd indicies.
    fn_bindings: *const FnTable,

    owned_pages: Vec<Page>,
    shared_pages: Vec<Arc<Page>>,
}

impl ExecutionCtx {
    pub fn new(fn_bindings: *const FnTable) -> Self {
        let bindings = unsafe {
            let mut array = Box::<PageTable>::new_uninit();
            for i in array.assume_init_mut().iter_mut() {
                *i = None;
            }
            array.assume_init()
        };

        Self {
            bindings: bindings,
            fn_bindings: fn_bindings,
            owned_pages: Vec::new(),
            shared_pages: Vec::new(),
        }
    }

    pub fn bindings_ptr(&self) -> *const *mut u8 {
        unsafe { std::mem::transmute(self.bindings.as_ptr()) }
    }

    pub fn fn_bindings_ptr(&self) -> *const *mut u8 {
        unsafe { std::mem::transmute(self.fn_bindings) }
    }
}

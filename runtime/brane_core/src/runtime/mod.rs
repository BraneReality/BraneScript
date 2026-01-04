use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{
    ir,
    memory::{self, FnTable, Page, PageTable, Store},
};

pub trait LoadedModule {
    fn get_fn(&self, id: ir::FnId) -> Option<memory::FnId>;
    fn get_fn_by_name(&self, name: impl AsRef<str>) -> Option<memory::FnId>;
    fn get_fn_names(&self) -> impl Iterator<Item = &str>;
}

pub trait JitBackend: Default {
    type ModuleTy: LoadedModule;

    fn load(&self, module: ir::Module, store: &Runtime<Self>) -> anyhow::Result<ModuleId>;

    fn get_module(&self, module: ModuleId) -> Option<Arc<Self::ModuleTy>>;
}

pub struct Runtime<B: JitBackend> {
    backend: B,
    pub store: Store,
    name_to_module: RwLock<HashMap<String, ModuleId>>,
    uri_to_module: RwLock<HashMap<ir::Uri, ModuleId>>,
    module_sources: RwLock<HashMap<ModuleId, Arc<ir::Module>>>,
}

pub type ModuleId = usize;

impl<B: JitBackend> Runtime<B> {
    pub fn new() -> Self {
        Self {
            backend: B::default(),
            store: Store::new(),
            name_to_module: Default::default(),
            uri_to_module: Default::default(),
            module_sources: Default::default(),
        }
    }

    /// Initializes the job runner and starts executing queued jobs. Non blocking.
    pub fn start_workers(n: Option<usize>) {
        todo!();
    }

    pub fn new_execution_ctx(&self) -> ExecutionCtx {
        ExecutionCtx::new(self.store.fn_table_ptr())
    }

    pub fn load_module(&self, uri: ir::Uri, module: ir::Module) -> anyhow::Result<ModuleId> {
        let name = module.id.clone();
        let stored_src = Arc::new(module.clone());

        let id = self.backend.load(module, self)?;

        self.name_to_module.write().unwrap().insert(name, id);
        self.uri_to_module.write().unwrap().insert(uri, id);
        self.module_sources.write().unwrap().insert(id, stored_src);
        Ok(id)
    }

    pub fn get_module(&self, module: ModuleId) -> Option<Arc<B::ModuleTy>> {
        self.backend.get_module(module)
    }

    pub fn find_module_by_uri(&self, name: &ir::Uri) -> Option<ModuleId> {
        self.uri_to_module.read().unwrap().get(name).cloned()
    }

    pub fn find_module_by_name(&self, name: impl AsRef<str>) -> Option<ModuleId> {
        let name = name.as_ref();
        self.name_to_module.read().unwrap().get(name).cloned()
    }

    pub fn module_src(&self, module: ModuleId) -> Option<Arc<ir::Module>> {
        self.module_sources.read().unwrap().get(&module).cloned()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BindingError {
    NoFreeBindings,
}

#[repr(C)]
pub struct ExecutionCtx {
    bindings: Box<PageTable>,
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

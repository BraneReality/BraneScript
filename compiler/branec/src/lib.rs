use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

pub use branec_hir::hir::{self, Graph, GraphId};

#[salsa::db]
#[derive(Clone, Default)]
pub struct ProjectCtx {
    storage: salsa::Storage<Self>,
    //pub workspace: Arc<PathBuf>,
    //pub path_ids: Arc<RwLock<HashMap<PathBuf, GraphId>>>,
    //pub loaded_graphs: Arc<RwLock<HashMap<GraphId, LoadedGraphCtx>>>,
}

#[salsa::db]
impl salsa::Database for ProjectCtx {}

pub enum GraphLoadMode {
    Mutable,
    Immutable,
}

pub struct LoadedGraphCtx {
    pub graph: RwLock<Graph>,
    pub load_mode: GraphLoadMode,
}

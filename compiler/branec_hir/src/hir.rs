use std::{collections::HashMap, hash::Hash};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefId {
    /// Runtime id for a brane
    pub graph: GraphId,
    /// Id of a defintion within a brane
    pub item: ItemId,
}

pub struct Project {
    pub graphs: HashMap<GraphId, Graph>,
    id_count: usize,
}

impl Project {
    pub fn new() -> Self {
        Self {
            graphs: HashMap::new(),
            id_count: 1usize,
        }
    }

    pub fn new_graph(&mut self) -> GraphId {
        let id = self.id_count;
        self.id_count += 1;

        let graph = Graph::default();

        self.graphs.insert(id, graph);
        id
    }

    pub fn graph(&self, id: GraphId) -> Option<&Graph> {
        self.graphs.get(&id)
    }

    pub fn graph_mut(&mut self, id: GraphId) -> Option<&mut Graph> {
        self.graphs.get_mut(&id)
    }

    pub fn get_item(&self, id: DefId) -> Option<&Item> {
        self.graph(id.graph)
            .map(|graph| graph.item(id.item))
            .flatten()
    }

    pub fn get_item_mut(&mut self, id: DefId) -> Option<&mut Item> {
        self.graph_mut(id.graph)
            .map(|graph| graph.item_mut(id.item))
            .flatten()
    }
}

pub struct Pipe {
    pub id: ItemId,
    pub sig: BlockSig,
}

pub struct Fn {
    pub id: ItemId,
    pub ident: String,
    pub sig: BlockSig,
    pub body: BlockId,
}

pub struct Extern {
    pub id: ItemId,
    pub sig: BlockSig,
}

pub struct Trait {
    pub id: ItemId,
    pub functions: Vec<(String, BlockSig)>,
}

pub struct TraitImpl {
    pub id: ItemId,
}

pub struct BlockSig {
    pub inputs: Vec<()>,
    pub outputs: Vec<()>,
}

#[derive(Copy, Clone)]
pub enum LocalValue {
    BrokenRef,
    BlockInput { index: usize },
    NodeOutput { node: NodeId, index: usize },
}

pub type BlockId = usize;
pub struct Block {
    pub id: BlockId,
    pub outputs: Vec<LocalValue>,
    pub nodes: HashMap<NodeId, Node>,
    pub id_counter: usize,
}

impl Block {}

pub type NodeId = usize;
pub struct Node {
    /// Local inputs that this node consumes
    pub inputs: Vec<LocalValue>,
    /// Id of Fn Item to execute
    pub expr: DefId,
}

pub type ItemId = usize;
pub enum Item {
    /// Pipeline definition
    Pipe(Pipe),
    /// Function definition
    Fn(Fn),
    /// Trait
    Trait(Trait),
    /// Trait Impl
    TraitImpl(TraitImpl),
}

/// This is one unit of code, that can be serialized in to a file.
pub type GraphId = usize;
pub struct Graph {
    id_count: usize,
    /// Map runtime ids to vector indices to allow for reordering without breaking references
    ids: HashMap<ItemId, usize>,
    /// Items defined in this brane
    items: Vec<Item>,
    blocks: HashMap<BlockId, Block>,
}

impl Default for Graph {
    fn default() -> Self {
        Self {
            id_count: 0,
            ids: Default::default(),
            items: Default::default(),
            blocks: Default::default(),
        }
    }
}

impl Graph {
    /// Add function item to the graph
    pub fn add_fn(&mut self, ident: String, sig: BlockSig, body: BlockId) -> ItemId {
        let id = self.id_count;
        self.id_count += 1;
        self.ids.insert(id, self.items.len());

        self.items.push(Item::Fn(Fn {
            id,
            ident,
            sig,
            body,
        }));
        id
    }

    /// Get an item in the graph
    pub fn item(&self, id: ItemId) -> Option<&Item> {
        if let Some(index) = self.item_index(id) {
            self.items.get(index)
        } else {
            None
        }
    }

    /// Get an item in the graph mutably
    pub fn item_mut(&mut self, id: ItemId) -> Option<&mut Item> {
        if let Some(index) = self.item_index(id) {
            self.items.get_mut(index)
        } else {
            None
        }
    }

    /// Swap the position of two items in the graph, true on success
    pub fn swap_items(&mut self, a: ItemId, b: ItemId) -> bool {
        if let (Some(ia), Some(ib)) = (self.item_index(a), self.item_index(a)) {
            self.ids.insert(a, ib);
            self.ids.insert(b, ia);
            self.items.swap(ib, ia);
            true
        } else {
            false
        }
    }

    /// Delete an item, returns the deleted item if successful
    pub fn delete_item(&mut self, id: ItemId) -> Option<Item> {
        if let Some(index) = self.item_index(id) {
            let item = self.items.swap_remove(index);
            if let Some(swapped) = self.items.get(index) {
                self.ids.insert(swapped.id(), index);
            }
            self.ids.remove(&id);
            Some(item)
        } else {
            None
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = self.id_count;
        self.id_count += 1;

        self.blocks.insert(
            id,
            Block {
                id,
                outputs: vec![],
                nodes: HashMap::new(),
                id_counter: 0,
            },
        );
        id
    }

    pub fn block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(&id)
    }

    pub fn block_mut(&mut self, id: BlockId) -> Option<&mut Block> {
        self.blocks.get_mut(&id)
    }

    /// Get the position of an item
    pub fn item_index(&self, id: ItemId) -> Option<usize> {
        self.ids.get(&id).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Item> {
        self.items.iter_mut()
    }
}

impl Item {
    pub fn id(&self) -> ItemId {
        match self {
            Item::Pipe(pipe) => pipe.id,
            Item::Fn(fn_) => fn_.id,
            Item::Trait(trait_) => trait_.id,
            Item::TraitImpl(trait_impl) => trait_impl.id,
        }
    }
}

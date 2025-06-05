use std::collections::HashMap;

pub type GraphId = usize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ItemId {
    pub id: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefId {
    /// Runtime id for a brane
    pub graph: GraphId,
    /// Id of a defintion within a brane
    pub id: ItemId,
}

/// This is one unit of code, that can be serialized in to a file.
pub struct Graph {
    id_count: usize,
    /// Map runtime ids to vector indices to allow for reordering without breaking references
    ids: HashMap<ItemId, usize>,
    /// Items defined in this brane
    items: Vec<Item>,
}

#[salsa::input]
pub struct Pipe {
    pub id: ItemId,
    pub sig: BlockSig,
}

#[salsa::input]
pub struct Fn {
    pub id: ItemId,
    pub sig: BlockSig,
}

#[salsa::input]
pub struct Extern {
    pub id: ItemId,
    pub sig: BlockSig,
}

#[salsa::input]
pub struct Trait {
    pub id: ItemId,
    pub functions: Vec<(String, BlockSig)>,
}

#[salsa::input]
pub struct TraitImpl {
    pub id: ItemId,
}

#[salsa::input]
pub struct BlockSig {
    #[returns(ref)]
    pub inputs: Vec<()>,
    #[returns(ref)]
    pub outputs: Vec<()>,
}

pub type NodeId = usize;
#[derive(Copy, Clone, Debug)]
pub enum LocalValue {
    BlockInput { index: usize },
    NodeOutput { node: NodeId, index: usize },
}

#[salsa::input]
pub struct Block {
    #[returns(ref)]
    pub outputs: Vec<LocalValue>,
    #[returns(ref)]
    pub nodes: HashMap<NodeId, Node>,
}

#[salsa::input]
pub struct Node {
    pub id: NodeId,
    /// Local inputs that this node consumes
    #[returns(ref)]
    pub inputs: Vec<LocalValue>,
    /// Id of Fn Item to execute
    pub expr: DefId,
}

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

impl Graph {
    /// Add item to the graph
    pub fn add_item(&mut self, item: Item) -> ItemId {
        let id = ItemId { id: self.id_count };
        self.id_count += 1;
        self.ids.insert(id, self.items.len());
        self.items.push(item);
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
    pub fn delete_item(&mut self, id: ItemId, db: &impl salsa::Database) -> Option<Item> {
        if let Some(index) = self.item_index(id) {
            let item = self.items.swap_remove(index);
            if let Some(swapped) = self.items.get(index) {
                self.ids.insert(swapped.id(db), index);
            }
            self.ids.remove(&id);
            Some(item)
        } else {
            None
        }
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
    pub fn id(&self, db: &impl salsa::Database) -> ItemId {
        match self {
            Item::Pipe(pipe) => pipe.id(db),
            Item::Fn(fn_) => fn_.id(db),
            Item::Trait(trait_) => trait_.id(db),
            Item::TraitImpl(trait_impl) => trait_impl.id(db),
        }
    }
}

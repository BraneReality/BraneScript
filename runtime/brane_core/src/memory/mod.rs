use bimap::BiMap;
use region::Allocation;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::atomic::AtomicUsize;
use std::{
    ptr::NonNull,
    sync::{Arc, Mutex, RwLock},
};

pub const BS_PAGE_SIZE: usize = u16::MAX as usize;

// Both these types are api safe, in accordance with the "null pointer optimization"
pub type PageTable = [Option<NonNull<u8>>; BS_PAGE_SIZE];
pub type PageId = u64;
pub type FnId = u64;

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct BSPtr(pub u32);

impl BSPtr {
    pub fn new(binding: u16, offset: u16) -> Self {
        Self(((binding as u32) << 8) & offset as u32)
    }

    pub fn binding(self) -> u16 {
        (self.0 >> 8) as u16
    }

    pub fn offset(self) -> u16 {
        (self.0 & 0x0000FFFF) as u16
    }

    pub fn set_binding(&mut self, binding: u16) {
        self.0 = ((binding as u32) << 8) & self.offset() as u32;
    }

    pub fn set_offset(&mut self, offset: u16) {
        self.0 = (((self.binding() as u32) << 8) & offset as u32) as u32;
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct BSPageHandle {
    pub page: u64,
    pub byte_offset: u16,
}

/// Either a PageId or a PageVec. If it's less than u64::MAX / 2 it's a PageId, if more it's a
/// PageVecId
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct BSMemHandle {
    pub mem_id: u64,
}

impl BSMemHandle {
    pub fn from_page(page: PageId) -> BSMemHandle {
        if page >= u64::MAX / 2 {
            panic!("Somehow we allocated vastly more pages then there are grains of sand on earth during the execution of this program");
        }

        BSMemHandle { mem_id: page }
    }

    pub fn from_page_vec(vec_id: u64) -> BSMemHandle {
        let delim = u64::MAX / 2;
        if vec_id >= delim {
            panic!("Somehow we allocated vastly more page vecs then there are grains of sand on earth during the execution of this program");
        }

        BSMemHandle {
            mem_id: vec_id + delim,
        }
    }

    pub fn value(self) -> MemId {
        let delim = u64::MAX / 2;
        if self.mem_id >= delim {
            MemId::PageVecId(self.mem_id - delim)
        } else {
            MemId::PageId(self.mem_id)
        }
    }
}

pub enum MemId {
    PageId(PageId),
    PageVecId(u64),
}

pub struct BindingSet {
    bindings: Box<PageTable>,
    fn_bindings: Box<PageTable>,
    page_binding_map: BiMap<PageId, u16>,

    owned_pages: Vec<Page>,
    shared_pages: Vec<Arc<Page>>,
}

impl BindingSet {
    pub fn new() -> Self {
        let bindings = unsafe {
            let mut array = Box::<PageTable>::new_uninit();
            for i in array.assume_init_mut().iter_mut() {
                *i = None;
            }
            array.assume_init()
        };

        let fn_bindings = unsafe {
            let mut array = Box::<PageTable>::new_uninit();
            for i in array.assume_init_mut().iter_mut() {
                *i = None;
            }
            array.assume_init()
        };

        Self {
            bindings: bindings,
            fn_bindings: fn_bindings,
            page_binding_map: BiMap::default(),
            owned_pages: Vec::new(),
            shared_pages: Vec::new(),
        }
    }

    pub fn bindings_ptr(&self) -> *const *mut u8 {
        unsafe { std::mem::transmute(self.bindings.as_ptr()) }
    }

    pub fn fn_bindings_ptr(&self) -> *const *mut u8 {
        unsafe { std::mem::transmute(self.fn_bindings.as_ptr()) }
    }

    fn find_free_bindings_in_range(
        &mut self,
        binding_count: usize,
        search_region: Range<usize>,
    ) -> Option<(Range<usize>, &mut [Option<NonNull<u8>>])> {
        for start in search_region {
            let end = start + binding_count;
            let check_range = start..end;
            if self.bindings[check_range.clone()]
                .iter()
                .all(|b| b.is_none())
            {
                return Some((check_range.clone(), &mut self.bindings[check_range]));
            }
        }
        None
    }

    pub fn bind_page_mutable(&mut self, mut page: Page) -> Result<u16, BindingError> {
        match self.find_free_bindings_in_range(1, 0..(u16::MAX / 2) as usize) {
            Some((range, bindings)) => {
                bindings[0] = NonNull::new(page.data.as_mut_ptr());
                self.owned_pages.push(page);
                Ok(range.start as u16)
            }
            None => Err(BindingError::NoFreeBindings),
        }
    }

    pub fn bind_page_immutable(&mut self, mut page: Page) -> Result<u16, BindingError> {
        match self.find_free_bindings_in_range(1, u16::MAX as usize / 2..u16::MAX as usize) {
            Some((range, bindings)) => {
                bindings[0] = NonNull::new(page.data.as_mut_ptr());
                self.owned_pages.push(page);
                Ok(range.start as u16)
            }
            None => Err(BindingError::NoFreeBindings),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BindingError {
    NoFreeBindings,
}

pub struct Page {
    pub id: PageId,
    pub data: Allocation,
}

pub struct PageVec {
    pub pages: Vec<PageId>,
}

#[derive(Default, Debug)]
pub struct AllocStats {
    pub total_alloc: AtomicUsize,
    pub total_alloc_unused: AtomicUsize,
}

pub struct Store {
    page_id_counter: AtomicUsize,
    unused_pages: Mutex<Vec<Page>>,
    shared_pages: RwLock<HashMap<PageId, Arc<Page>>>,
    stats: Arc<AllocStats>,
}

impl Store {
    pub fn new() -> Self {
        Self {
            unused_pages: Mutex::new(Vec::new()),
            page_id_counter: AtomicUsize::new(0),
            shared_pages: RwLock::new(HashMap::new()),
            stats: Arc::default(),
        }
    }

    /// Returns an unused page
    pub fn alloc_page(&self) -> Page {
        let id = self
            .page_id_counter
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed) as PageId;

        if let Some(mut page) = self.unused_pages.lock().unwrap().pop() {
            page.id = id;
            self.stats
                .total_alloc_unused
                .fetch_sub(page.data.len(), std::sync::atomic::Ordering::Relaxed);
            return page;
        }

        let data = region::alloc(BS_PAGE_SIZE, region::Protection::READ_WRITE)
            .expect("Page allocation failed");
        self.stats
            .total_alloc
            .fetch_add(data.len(), std::sync::atomic::Ordering::Relaxed);
        Page { id, data }
    }

    /// Adds a page to the unused pool
    pub fn free_page(&self, page: Page) {
        self.stats
            .total_alloc_unused
            .fetch_add(page.data.len(), std::sync::atomic::Ordering::Relaxed);
        self.unused_pages.lock().unwrap().push(page);
    }

    pub fn get_stats(&self) -> Arc<AllocStats> {
        self.stats.clone()
    }
}

#[derive(Debug)]
pub enum PtrError {
    NotBound,
    Unaligned,
}

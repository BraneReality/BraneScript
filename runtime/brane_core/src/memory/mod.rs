use bimap::BiMap;
use region::Allocation;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::atomic::{AtomicU16, AtomicU32, AtomicUsize};
use std::{
    ptr::NonNull,
    sync::{Arc, Mutex, RwLock},
};

pub const BS_PAGE_SIZE: usize = u16::MAX as usize;

// Both these types are api safe, in accordance with the "null pointer optimization"
pub type PageTable = [Option<NonNull<u8>>; BS_PAGE_SIZE];
pub type FnTable = [Option<NonNull<usize>>; BS_PAGE_SIZE];
/// Currently an alias for global binding index
pub type PageId = u16;
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

pub struct Page {
    pub id: PageId,
    pub data: Allocation,
}

pub struct PageVec {
    pub pages: Vec<PageId>,
}

pub struct Store {
    unused_pages: Mutex<Vec<Page>>,
    next_page_binding: AtomicU32,
}

impl Store {
    pub fn new() -> Self {
        Self {
            unused_pages: Default::default(),
            next_page_binding: AtomicU32::new(1),
        }
    }

    /// Returns an unused page
    pub fn alloc_page(&self) -> Result<Page, AllocationError> {
        if let Some(page) = self.unused_pages.lock().unwrap().pop() {
            return Ok(page);
        }

        let data = region::alloc(BS_PAGE_SIZE, region::Protection::READ_WRITE)
            .map_err(|_| AllocationError::OsFailedToAllocatePage)?;

        let id = self
            .next_page_binding
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if id == std::u16::MAX as u32 {
            return Err(AllocationError::OutOfMemory);
        }
        let id = id as u16;
        Ok(Page { id, data })
    }

    /// Adds a page to the unused pool
    pub fn free_page(&self, page: Page) {
        self.unused_pages.lock().unwrap().push(page);
    }
}

#[derive(Debug)]
pub enum AllocationError {
    OutOfMemory,
    OsFailedToAllocatePage,
}

#[derive(Debug)]
pub enum PtrError {
    NotBound,
    Unaligned,
}

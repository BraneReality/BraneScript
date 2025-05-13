use std::{collections::HashMap, sync::RwLock};

struct Symbols {
    pub symbol_to_id: HashMap<&'static str, usize>,
    pub symbol_str: Vec<&'static str>,
    pub storage: Vec<Box<String>>,
}

static SYMBOLS: std::sync::LazyLock<RwLock<Symbols>> = std::sync::LazyLock::new(|| {
    RwLock::new(Symbols {
        symbol_to_id: HashMap::new(),
        symbol_str: Vec::new(),
        storage: Vec::new(),
    })
});

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Symbol {
    pub id: usize,
}

impl Symbol {
    pub fn intern(text: &str) -> Symbol {
        {
            let symbols = SYMBOLS.read().expect("symbol interner failed to init");
            if let Some(symbol) = symbols.symbol_to_id.get(text) {
                return Symbol { id: *symbol };
            }
        }
        let mut symbols = SYMBOLS.write().expect("symbol interner failed to init");
        let value = Box::new(text.to_string());
        let str: &'static str = unsafe { std::mem::transmute(value.as_str()) };
        let id = symbols.storage.len();
        symbols.symbol_to_id.insert(str, id);
        symbols.symbol_str.push(str);
        symbols.storage.push(value);
        Symbol { id }
    }

    pub fn as_str(&self) -> &'static str {
        let symbols = SYMBOLS.read().expect("symbol interner failed to init");
        if symbols.symbol_str.len() <= self.id {
            return "invalid symbol";
        }
        symbols.symbol_str[self.id]
    }
}

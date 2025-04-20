extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Ident, Token, parse_macro_input, punctuated::Punctuated};

struct TableMembers {
    type_kind: Ident,
    _comma: Token![,],
    members: Punctuated<Ident, Token![,]>,
}

impl syn::parse::Parse for TableMembers {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let table_name = input.parse()?;
        let _comma = input.parse()?;
        let members = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        Ok(TableMembers {
            type_kind: table_name,
            _comma,
            members,
        })
    }
}

#[proc_macro]
pub fn arena_with_types(types: TokenStream) -> TokenStream {
    let input = parse_macro_input!(types as TableMembers);

    let enum_members = input.members.iter().fold(quote! {}, |stream, id| {
        quote! {
            #stream
            #id(#id<'a>),
        }
    });

    let table_name = format_ident!("{}Kind", input.type_kind);
    let arena_type = format_ident!("{}Arena", input.type_kind);

    let alloc_funcs = input.members.iter().fold(quote! {}, |stream, id| {
        let fn_name = format_ident!("alloc_{}", id.to_string().to_lowercase());
        quote! {
            #stream
            pub fn #fn_name (&self, value: #id <'a>) -> &mut #id <'a> {
                if let #table_name::#id(value) = &mut *self.0.alloc(#table_name::#id(value)) {
                    value
                } else {
                    unreachable!("variant type should be known")
                }
            }
        }
    });

    TokenStream::from(quote! {
        pub enum #table_name <'a> {
            #enum_members
        }

        pub struct #arena_type <'a>(pub Arena<#table_name<'a>>);

        impl<'a> #arena_type <'a> {
            pub fn new() -> Self {
                Self(Arena::new())
            }

            #alloc_funcs
        }
    })
}

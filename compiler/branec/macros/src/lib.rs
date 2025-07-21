extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{DeriveInput, LitStr, parse_macro_input};

#[proc_macro_derive(CompilerValueApi)]
pub fn compiler_value_alias_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Generate the implementation for either struct or enum
    TokenStream::from(match &input.data {
        syn::Data::Struct(object) => {
            let name = &input.ident;

            let fields_to_values = match &object.fields {
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("Field must have ident");
                        let label = LitStr::new(ident.to_string().as_str(), input.ident.span());
                        let ty = &field.ty;
                        quote! {
                            ObjectMember{label:#label.into(), value: <#ty as CompilerValueApi>::to_value(&self.#ident) }
                        }
                    })
                    .reduce(|a, b| {
                        quote! { #a, #b }
                    })
                    .unwrap_or(quote! {}),
                _ => {
                    quote_spanned! { input.ident.span() => compiler_error!("CompilerValueApi can only be derived for structs with named fields") }
                }
            };

            let object_to_fields = match &object.fields {
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("Field must have ident");
                        let label = LitStr::new(
                            ident.to_string()
                                .as_str(),
                            input.ident.span(),
                        );
                        let ty = &field.ty;
                        quote! {
                            #ident: <#ty as CompilerValueApi>::from_value(object.get(#label).map(|m| m.value.clone()).ok_or_else(||{
                                Error {
                                    stack_trace: vec![],
                                    message: #label.into(),
                                }
                            })?)?
                        }
                    })
                    .reduce(|a, b| {
                        quote! { #a, #b }
                    })
                    .unwrap_or(quote! {}),
                _ => {
                    quote_spanned! { input.ident.span() => compiler_error!("CompilerValueApi can only be derived for structs with named fields") }
                }
            };
            let (field_idents, field_types): (Vec<_>, Vec<_>) = match &object.fields {
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().unwrap();
                        let ty = &field.ty;
                        (ident, ty)
                    })
                    .unzip(),
                _ => {
                    return quote_spanned! { input.ident.span() => compiler_error!("CompilerValueApi can only be derived for structs with named fields") }.into();
                }
            };

            quote! {
                impl CompilerValueApi for #name {
                    fn to_value(&self) -> CompilerValue {
                        CompilerValue::object([#fields_to_values])
                    }

                    fn from_value(value: CompilerValue) -> Result<Self, Error> {
                        let Some(object) = value.as_object() else {
                            return Err(Error {
                                stack_trace: vec![],
                                message: format!("Expected an object but found {:?}", value),
                            });
                        };
                        Ok(Self {
                            #object_to_fields
                        })
                    }

                    fn spec() -> CompilerValue {
                        CompilerValue::variant("Struct", Some(CompilerValue::object([
                            ObjectMember {
                                label: "members".into(),
                                value: CompilerValue::array(vec![
                                    #(
                                        CompilerValue::object([
                                            ObjectMember {
                                                label: "label".into(),
                                                value: CompilerValue::string(stringify!(#field_idents)),
                                            },
                                            ObjectMember {
                                                label: "spec".into(),
                                                value: CompilerValue::lazy(|| <#field_types as CompilerValueApi>::spec()),
                                            },
                                        ])
                                    ),*
                                ]),
                            },
                            ObjectMember {
                                label: "strictness".into(),
                                value: CompilerValue::variant("Unordered", None),
                            }
                        ])))
                    }

                }
            }
        }
        syn::Data::Enum(object) => {
            let name = &input.ident;

            // For each variant
            let to_arms = object.variants.iter().map(|variant| {
                let variant_ident = &variant.ident;
                let label = variant_ident.to_string();
                match &variant.fields {
                    syn::Fields::Unit => quote! {
                        Self::#variant_ident => CompilerValue::variant(#label, None)
                    },
                    syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => quote! {
                        Self::#variant_ident(inner) => CompilerValue::variant(#label, Some(inner.to_value()))
                    },
                    _ => quote_spanned! { variant.ident.span() =>
                        compile_error!("Only unit and single-value tuple variants are supported in enums for CompilerValueApi")
                    },
                }
            });

            let from_arms = object.variants.iter().map(|variant| {
                let variant_ident = &variant.ident;
                let label = variant_ident.to_string();
                match &variant.fields {
                    syn::Fields::Unit => quote! {
                        #label => Ok(Self::#variant_ident)
                    },
                    syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => quote! {
                        #label => {
                            let inner = value.value.ok_or_else(|| Error {
                                stack_trace: vec![],
                                message: format!("Expected value for enum variant {}", #label),
                            })?;
                            Ok(Self::#variant_ident(<_ as CompilerValueApi>::from_value(*inner)?))
                        }
                    },
                    _ => quote_spanned! { variant.ident.span() =>
                        compile_error!("Only unit and single-value tuple variants are supported in enums for CompilerValueApi")
                    },
                }
            });

            let (variant_idents, variant_specs): (Vec<_>, Vec<_>) = object
                .variants
                .iter()
                .map(|variant| {
                    let ident = &variant.ident;
                    match &variant.fields {
                        syn::Fields::Unit => (ident, quote! { None }),
                        syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                            let ty = &fields.unnamed.first().unwrap().ty;
                            (
                                ident,
                                quote! {
                                    Some(CompilerValue::lazy(||  <#ty as CompilerValueApi>::spec()))
                                },
                            )
                        }
                        _ => panic!("Unsupported enum variant for spec() in CompilerValueApi"),
                    }
                })
                .unzip();

            quote! {
                impl CompilerValueApi for #name {
                    fn to_value(&self) -> CompilerValue {
                        match self {
                            #(#to_arms),*
                        }
                    }

                    fn from_value(value: CompilerValue) -> Result<Self, Error> {
                        let CompilerValueKind::EnumVariant(value) = value.kind else {
                            return Err(Error {
                                stack_trace: vec![],
                                message: format!("Expected an enum variant but found {:?}", value),
                            });
                        };

                        match value.label.as_str() {
                            #(#from_arms),*,
                            _ => Err(Error {
                                stack_trace: vec![],
                                message: format!("Unknown enum variant label: {}", value.label),
                            }),
                        }
                    }

                    fn spec() -> CompilerValue {
                        CompilerValue::variant("Options", Some(CompilerValue::array(vec![
                            #(
                                CompilerValue::variant(stringify!(#variant_idents), {
                                    #variant_specs
                                })
                            ),*
                        ])))
                    }
                }
            }
        }
        _ => {
            quote_spanned! { input.ident.span() => compiler_error!("CompilerValueApi can only be derived for structs and enums") }
        }
    })
}

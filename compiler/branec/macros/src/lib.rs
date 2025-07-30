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
            let name_str = name.to_string();

            let fields_to_values = match &object.fields {
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("Field must have ident");
                        let label = LitStr::new(ident.to_string().as_str(), input.ident.span());
                        let ty = &field.ty;
                        quote! {
                            ObjectMember::new(#label, <#ty as CompilerValueApi>::as_value(&self.#ident))
                        }
                    })
                    .reduce(|a, b| {
                        quote! { #a, #b }
                    })
                    .unwrap_or(quote! {}),
                _ => {
                    quote_spanned! { input.ident.span() => compile_error!("CompilerValueApi can only be derived for structs with named fields") }
                }
            };

            let object_to_fields = match &object.fields {
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|field| {
                        let ident = field.ident.as_ref().expect("Field must have ident");
                        let label = LitStr::new(ident.to_string().as_str(), input.ident.span());
                        let ty = &field.ty;
                        quote! {
                            #ident: <#ty as CompilerValueApi>::from_value(

                                &object.get(#label).ok_or_else(|| {
                                    Error::new(format!("Missing field: {}", #label))
                                })?.value
                            )?
                        }
                    })
                    .reduce(|a, b| {
                        quote! { #a, #b }
                    })
                    .unwrap_or(quote! {}),
                _ => {
                    quote_spanned! { input.ident.span() => compile_error!("CompilerValueApi can only be derived for structs with named fields") }
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
                    return quote_spanned! { input.ident.span() => compile_error!("CompilerValueApi can only be derived for structs with named fields") }.into();
                }
            };

            // Generate type representation - same shape as as_value but with type values
            let fields_to_type_values = field_types
                .iter()
                .zip(field_idents.iter())
                .map(|(ty, ident)| {
                    let label = LitStr::new(ident.to_string().as_str(), input.ident.span());
                    let ty_str = quote!(#ty).to_string();
                    if ty_str.contains(&name_str) || ty_str.contains("Self") {
                        quote! {
                            ObjectMember::new(#label, Value::Recursive(#name_str.into()))
                        }
                    } else {
                        quote! {
                            ObjectMember::new(#label, <#ty as CompilerValueApi>::ty())
                        }
                    }
                })
                .reduce(|a, b| {
                    quote! { #a, #b }
                })
                .unwrap_or(quote! {});

            quote! {
                impl CompilerValueApi for #name {
                    fn as_value(&self) -> Value {
                        Value::Object(Object::new(
                            Some(#name_str.to_string()),
                            vec![#fields_to_values]
                        ))
                    }

                    fn from_value(value: &Value) -> Result<Self, Error> {
                        let Value::Object(object) = value else {
                            return Err(Error::new(format!(
                                "Expected an object but found {:?}",
                                value
                            )));
                        };
                        Ok(Self {
                            #object_to_fields
                        })
                    }

                    fn ty() -> Value {
                        Value::Object(Object::new(
                            Some(#name_str.to_string()),
                            vec![#fields_to_type_values]
                        ))
                    }
                }
            }
        }
        syn::Data::Enum(object) => {
            let name = &input.ident;
            let name_str = name.to_string();

            // For each variant in as_value - only one variant will be present
            let to_arms = object.variants.iter().map(|variant| {
                let variant_ident = &variant.ident;
                let label = variant_ident.to_string();
                match &variant.fields {
                    syn::Fields::Unit => quote! {
                        Self::#variant_ident => Value::Enum(Enum {
                            label: Some(#name_str.to_string()),
                            variants: vec![EnumVariant {
                                label: #label.to_string(),
                                value: None,
                            }]
                        })
                    },
                    syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => quote! {
                        Self::#variant_ident(inner) => Value::Enum(Enum {
                            label: Some(#name_str.to_string()),
                            variants: vec![EnumVariant {
                                label: #label.to_string(),
                                value: Some(inner.as_value()),
                            }]
                        })
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
                    syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        let ty = &fields.unnamed.first().unwrap().ty;
                        quote! {
                            #label => {
                                let inner = variant.value.clone().ok_or_else(|| Error::new(format!(
                                    "Expected value for enum variant {}", 
                                    #label
                                )))?;
                                Ok(Self::#variant_ident(<#ty as CompilerValueApi>::from_value(&inner)?))
                            }
                        }
                    },
                    _ => quote_spanned! { variant.ident.span() =>
                        compile_error!("Only unit and single-value tuple variants are supported in enums for CompilerValueApi")
                    },
                }
            });

            // For ty() - all variants should be present
            let all_variant_specs: Vec<_> = object
                .variants
                .iter()
                .map(|variant| {
                    let ident = &variant.ident;
                    let ident_str = ident.to_string();
                    match &variant.fields {
                        syn::Fields::Unit => quote! {
                            EnumVariant {
                                label: #ident_str.to_string(),
                                value: None,
                            }
                        },
                        syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                            let ty = &fields.unnamed.first().unwrap().ty;
                            let ty_str = quote!(#ty).to_string();
                            if ty_str.contains(&name_str) || ty_str.contains("Self") {
                                quote! {
                                    EnumVariant {
                                        label: #ident_str.to_string(),
                                        value: Some(Value::Recursive(#name_str.into())),
                                    }
                                }
                            } else {
                                quote! {
                                    EnumVariant {
                                        label: #ident_str.to_string(),
                                        value: Some(<#ty as CompilerValueApi>::ty()),
                                    }
                                }
                            }
                        }
                        _ => panic!("Unsupported enum variant for ty() in CompilerValueApi"),
                    }
                })
                .collect();

            quote! {
                impl CompilerValueApi for #name {
                    fn as_value(&self) -> Value {
                        match self {
                            #(#to_arms),*
                        }
                    }

                    fn from_value(value: &Value) -> Result<Self, Error> {
                        let Value::Enum(enum_value) = value else {
                            return Err(Error::new(format!(
                                "Expected an enum variant but found {:?}",
                                value
                            )));
                        };

                        // Assuming the enum has only one variant when deserializing
                        let variant = enum_value.variants.iter().next().ok_or_else(|| {
                            Error::new("Enum has no variants".to_string())
                        })?;

                        match variant.label.as_str() {
                            #(#from_arms),*,
                            _ => Err(Error::new(format!(
                                "Unknown enum variant label: {}",
                                variant.label
                            ))),
                        }
                    }

                    fn ty() -> Value {
                        Value::Enum(Enum {
                            label: Some(#name_str.to_string()),
                            variants: vec![
                                #(#all_variant_specs),*
                            ]
                        })
                    }
                }
            }
        }
        _ => {
            quote_spanned! { input.ident.span() => compile_error!("CompilerValueApi can only be derived for structs and enums") }
        }
    })
}

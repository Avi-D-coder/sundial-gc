use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, DataEnum, DataStruct, DeriveInput,
    Field, Fields, FieldsNamed, FieldsUnnamed, Type, Variant,
};

#[proc_macro_derive(Trace)]
pub fn derive_answer_fn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let DeriveInput {
        ident,
        generics,
        data,
        ..
    } = input;

    let where_clause = generics.where_clause.clone();
    // if generics.lifetimes().count() > 1 {
    //     panic!("Only one lifetime supported")
    // }

    let tuple = |unnamed: Punctuated<Field, Comma>, types: &mut Vec<Type>| {
        let args: Vec<_> = unnamed
            .iter()
            .enumerate()
            .map(|(i, Field { ty, .. })| {
                let arg = quote! {f#i, offset #( + <#types>::GC_COUNT)*};
                types.push(ty.clone());
                arg
            })
            .collect();

        let f = quote! {
            let mut r = 0b0000_0000;
            #(r |= Trace::fields(#args, grey_fields, invariant); )*
            r
        };
        let e = quote! {
            #(Trace::evacuate(#args, grey_fields, invariant, handlers); )*
        };

        (f, e)
    };

    let struc = |named: Punctuated<Field, Comma>, types: &mut Vec<Type>| {
        let args: Vec<_> = named
            .iter()
            .map(|Field { ty, ident, .. }| {
                let ident = ident.as_ref().unwrap();
                let arg = quote! {#ident, offset #( + <#types>::GC_COUNT)*};
                types.push(ty.clone());
                arg
            })
            .collect();

        let f = quote! {
            let mut r = 0b0000_0000;
            #(r |= Trace::fields(#args, grey_fields, invariant); )*
            r
        };
        let e = quote! {
            #(Trace::evacuate(#args, grey_fields, invariant, handlers); )*
        };

        (f, e)
    };

    let mut types: Vec<Type> = vec![];

    let (fields, evacuate) = match data {
        syn::Data::Struct(DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(FieldsNamed { named, .. }) => struc(named, &mut types),
            syn::Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => tuple(unnamed, &mut types),
            syn::Fields::Unit => (quote! {}, quote! {}),
        },

        syn::Data::Enum(DataEnum { variants, .. }) => {
            let (f_arms, e_arms): (Vec<_>, Vec<_>) = variants
                .into_iter()
                .filter_map(|Variant { ident, fields, .. }| match fields {
                    Fields::Named(FieldsNamed { named, .. }) => {
                        let names: Vec<_> = named
                            .iter()
                            .map(|Field { ident, .. }| ident.clone().unwrap())
                            .collect();
                        let (f, e) = struc(named, &mut types);
                        let f = quote! {
                            #ident{#(#names, )*} => {
                                #f
                            },
                        };

                        let e = quote! {
                            #ident{#(#names, )*} => {
                                #e
                            },
                        };

                        Some((f, e))
                    }
                    Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                        let names: Vec<_> = unnamed
                            .iter()
                            .enumerate()
                            .map(|(i, _)| quote! {f#i})
                            .collect();
                        let (f, e) = tuple(unnamed, &mut types);
                        let f = quote! {
                            #ident(#(#names, )*) => {
                                #f
                            },
                        };

                        let e = quote! {
                            #ident(#(#names, )*) => {
                                #e
                            },
                        };

                        Some((f, e))
                    }
                    Fields::Unit => None,
                })
                .unzip();

            let f = quote! {
                match s {
                    #(#f_arms)*
                }
            };

            let e = quote! {
                match s {
                    #(#e_arms)*
                }
            };

            (f, e)
        }
        syn::Data::Union(_) => panic!("Cannot derive Trace for a Union"),
    };

    let direct_gc_types = types
        .iter()
        .enumerate()
        .map(|(i, ty)| (ty, &types[..i]))
        .map(|(ty, prior)| quote! {<#ty>::direct_gc_types(t, offset #(+ #prior)*);});

    let expanded = quote! {
        unsafe impl #generics sundail_gc::Trace for #ident #generics #where_clause {
            fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &sundail_gc::mark::Invariant) -> u8 {
                let mut bloom = 0b0000000;
                #fields
                bloom
            }

            unsafe fn evacuate<'e>(
                s: &Self,
                offset: sundail_gc::mark::Offset,
                grey_fields: u8,
                invariant: &sundail_gc::mark::Invariant,
                handlers: &mut sundail_gc::mark::Handlers,
            ) {
                #evacuate
            }

            fn direct_gc_types(
                t: &mut std::collections::HashMap<sundail_gc::mark::GcTypeInfo, sundail_gc::mark::TypeRow>,
                offset: u8,
            ) {
                #(#direct_gc_types
                  )*
            }

            fn transitive_gc_types(tti: *mut sundail_gc::mark::Tti) {
                #(<#types>::transitive_gc_types(tti);
                  )*
            }

            const GC_COUNT: u8 = #(<#types>::GC_COUNT)+*;
            const PRE_CONDTION: bool = #(<#types>::PRE_CONDTION)+*;
        }
    };

    TokenStream::from(expanded)
}

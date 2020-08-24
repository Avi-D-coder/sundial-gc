use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, token::Comma, DataEnum, DataStruct,
    DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed, Ident, Lifetime, LifetimeDef, Type,
    Variant,
};

#[proc_macro_derive(Trace)]
pub fn derive_trace_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    proc_macro::TokenStream::from(trace_impl(input))
}

// TODO handle associated type constraints of the form List<'r, T: 'r>.
// The work around is to use a where clause where T: 'r
fn trace_impl(input: DeriveInput) -> TokenStream {
    let DeriveInput {
        ident: top_name,
        mut generics,
        data,
        ..
    } = input;

    generics.make_where_clause();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut generics_l = generics.clone();
    let (impl_generics_l, _, _) = generics.split_for_impl();

    let mut where_clause = where_clause.unwrap().clone();
    let mut where_clause_l = where_clause.clone();

    generics.type_params().for_each(|t| {
        where_clause
            .predicates
            .push(parse_quote! { #t: sundial_gc::Trace });

        where_clause_l
            .predicates
            .push(parse_quote! { #t: sundial_gc::mark::CoerceLifetime });
    });

    let tuple = |unnamed: Punctuated<Field, Comma>, types: &mut Vec<Type>| {
        let args: Vec<_> = unnamed
            .iter()
            .enumerate()
            .map(|(i, Field { ty, .. })| {
                let i = Ident::new(&format!("f{}", i), Span::call_site());
                let arg = quote! {#i, offset #( + <#types>::GC_COUNT)*};
                types.push(ty.clone());
                arg
            })
            .collect();

        let f = quote! {
            #(bloom |= Trace::fields(#args, grey_fields, invariant); )*
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
            #(bloom |= Trace::fields(#args, grey_fields, invariant); )*
        };
        let e = quote! {
            #(Trace::evacuate(#args, grey_fields, invariant, handlers); )*
        };

        (f, e)
    };

    let tuple_names = |unnamed: &Punctuated<Field, Comma>| -> Vec<_> {
        unnamed
            .iter()
            .enumerate()
            .map(|(i, _)| Ident::new(&format!("f{}", i), Span::call_site()))
            .map(|i| quote! {#i})
            .collect()
    };

    let struc_names = |named: &Punctuated<Field, Comma>| -> Vec<_> {
        named
            .iter()
            .map(|Field { ident, .. }| ident.clone().unwrap())
            .collect()
    };

    let mut types: Vec<Type> = vec![];

    let (fields, evacuate) = match data {
        syn::Data::Struct(DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(FieldsNamed { named, .. }) => {
                let names = struc_names(&named);
                let (f, e) = struc(named, &mut types);
                let f = quote! {
                    let Self {#(#names, )*} = s;
                    #f
                };

                let e = quote! {
                    let Self {#(#names, )*} = s;
                    #e
                };
                (f, e)
            }
            syn::Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let names = tuple_names(&unnamed);
                let (f, e) = tuple(unnamed, &mut types);
                let f = quote! {
                    let Self (#(#names,)* ) = s;
                    #f
                };

                let e = quote! {
                    let Self (#(#names, )*) = s;
                    #e
                };
                (f, e)
            }
            syn::Fields::Unit => (quote! {}, quote! {}),
        },

        syn::Data::Enum(DataEnum { variants, .. }) => {
            let (f_arms, e_arms): (Vec<_>, Vec<_>) = variants
                .into_iter()
                .filter_map(|Variant { ident, fields, .. }| match fields {
                    Fields::Named(FieldsNamed { named, .. }) => {
                        let names = struc_names(&named);
                        let (f, e) = struc(named, &mut types);
                        let f = quote! {
                            #top_name::#ident{#(#names, )*} => {
                                #f
                            },
                        };

                        let e = quote! {
                            #top_name::#ident{#(#names, )*} => {
                                #e
                            },
                        };

                        Some((f, e))
                    }
                    Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                        let names = tuple_names(&unnamed);
                        let (f, e) = tuple(unnamed, &mut types);
                        let f = quote! {
                            #top_name::#ident(#(#names, )*) => {
                                #f
                            },
                        };

                        let e = quote! {
                            #top_name::#ident(#(#names, )*) => {
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
                    _ => (),
                };
            };

            let e = quote! {
                match s {
                    #(#e_arms)*
                    _ => (),
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
        .map(|(ty, prior)| quote! {<#ty>::direct_gc_types(t, offset #(+ <#prior>::GC_COUNT)*);});

    let lifetime_r = generics.lifetimes().next();
    let lifetime_r = lifetime_r.iter();

    let lifetime = generics
        .lifetimes()
        .next()
        .map(|_| quote! {'coerce_lifetime,})
        .unwrap_or_default();

    let type_params = generics
        .type_params()
        .map(|t| t.ident.clone())
        .map(|t| quote! { #t::Type<'coerce_lifetime> });

    quote! {
        unsafe impl #impl_generics sundial_gc::Trace for #top_name #ty_generics #where_clause {
            default fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &sundial_gc::mark::Invariant) -> u8 {
                let mut bloom = 0b0000000;
                #fields
                bloom
            }

            default unsafe fn evacuate<'e>(
                s: &Self,
                offset: sundial_gc::mark::Offset,
                grey_fields: u8,
                invariant: &sundial_gc::mark::Invariant,
                handlers: &mut sundial_gc::mark::Handlers,
            ) {
                #evacuate
            }

            default fn direct_gc_types(
                t: &mut std::collections::HashMap<sundial_gc::mark::GcTypeInfo, sundial_gc::mark::TypeRow>,
                offset: u8,
            ) {
                #(#direct_gc_types
                  )*
            }

            default fn transitive_gc_types(tti: *mut sundial_gc::mark::Tti) {
                #(<#types>::transitive_gc_types(tti);
                  )*
            }

            default const GC_COUNT: u8 = #(<#types>::GC_COUNT)+*;
            default const PRE_CONDITION: bool = #(<#types>::PRE_CONDITION)&&*;
        }

       unsafe impl #impl_generics_l sundial_gc::mark::CoerceLifetime for #top_name #ty_generics #where_clause_l {
            type Type<'coerce_lifetime> = #top_name<#lifetime #(#type_params,)*>;
       }
    }
}

#[test]
fn binary_tree_derive_test() {
    let input: DeriveInput = parse_quote! {
         pub enum BinaryTree<'r, K, V> {
             Empty,
             Branch(Gc<'r, (K, Self, Self, V)>),
         }
    };

    let _ = trace_impl(input);
}

#[test]
fn add_to_where() {
    let mut w: syn::WhereClause = parse_quote! { where };
    let t: Ident = parse_quote! { T };
    w.predicates.push(parse_quote! { #t: sundial_gc::Trace });
    w.predicates.push(parse_quote! { #t: sundial_gc::Trace });
}

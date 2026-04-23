use proc_macro::TokenStream;
use quote::quote;
use std::collections::BTreeSet;
use syn::{Attribute, Ident};
use syn::{Data, DeriveInput, Fields, parse_macro_input};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum PolyZipperTrait {
    Zipper,
    ZipperValues,
    ZipperReadOnlyValues,
    ZipperReadOnlyConditionalValues,
    ZipperReadOnlyConditionalIteration,
    ZipperReadOnlyIteration,
    ZipperMoving,
    ZipperIteration,
    ZipperConcrete,
    ZipperAbsolutePath,
    ZipperPathBuffer,
    ZipperSubtries,
}

impl PolyZipperTrait {
    fn from_ident(ident: &Ident) -> Option<Self> {
        match ident.to_string().as_str() {
            "Zipper" => Some(Self::Zipper),
            "ZipperValues" => Some(Self::ZipperValues),
            "ZipperReadOnlyValues" => Some(Self::ZipperReadOnlyValues),
            "ZipperReadOnlyConditionalValues" => Some(Self::ZipperReadOnlyConditionalValues),
            "ZipperReadOnlyConditionalIteration" => Some(Self::ZipperReadOnlyConditionalIteration),
            "ZipperReadOnlyIteration" => Some(Self::ZipperReadOnlyIteration),
            "ZipperMoving" => Some(Self::ZipperMoving),
            "ZipperIteration" => Some(Self::ZipperIteration),
            "ZipperConcrete" => Some(Self::ZipperConcrete),
            "ZipperAbsolutePath" => Some(Self::ZipperAbsolutePath),
            "ZipperPathBuffer" => Some(Self::ZipperPathBuffer),
            "ZipperSubtries" => Some(Self::ZipperSubtries),
            _ => None,
        }
    }
}

fn all_poly_zipper_traits() -> BTreeSet<PolyZipperTrait> {
    use PolyZipperTrait::*;
    BTreeSet::from([
        Zipper,
        ZipperValues,
        ZipperReadOnlyValues,
        ZipperReadOnlyConditionalValues,
        ZipperReadOnlyConditionalIteration,
        ZipperReadOnlyIteration,
        ZipperMoving,
        ZipperIteration,
        ZipperConcrete,
        ZipperAbsolutePath,
        ZipperPathBuffer,
        ZipperSubtries,
    ])
}

fn add_trait_dependencies(traits: &mut BTreeSet<PolyZipperTrait>) {
    use PolyZipperTrait::*;
    let mut changed = true;
    while changed {
        changed = false;
        if traits.contains(&ZipperReadOnlyIteration) {
            changed |= traits.insert(ZipperReadOnlyValues);
            changed |= traits.insert(ZipperIteration);
        }
        if traits.contains(&ZipperReadOnlyConditionalIteration) {
            changed |= traits.insert(ZipperReadOnlyConditionalValues);
            changed |= traits.insert(ZipperIteration);
        }
        if traits.contains(&ZipperReadOnlyValues) && traits.insert(ZipperValues) {
            changed = true;
        }
        if traits.contains(&ZipperReadOnlyConditionalValues) && traits.insert(ZipperValues) {
            changed = true;
        }
        if traits.contains(&ZipperIteration) && traits.insert(ZipperMoving) {
            changed = true;
        }
        if traits.contains(&ZipperAbsolutePath) && traits.insert(ZipperMoving) {
            changed = true;
        }
        if traits.contains(&ZipperPathBuffer) && traits.insert(ZipperMoving) {
            changed = true;
        }
        if traits.contains(&ZipperMoving) && traits.insert(Zipper) {
            changed = true;
        }
        if traits.contains(&ZipperSubtries) && traits.insert(ZipperValues) {
            changed = true;
        }
    }
}

fn parse_explicit_traits(
    attrs: &[Attribute],
) -> Result<Option<BTreeSet<PolyZipperTrait>>, syn::Error> {
    let mut explicit: Option<BTreeSet<PolyZipperTrait>> = None;
    for attr in attrs {
        if !attr.path().is_ident("poly_zipper_explicit") {
            continue;
        }
        if explicit.is_some() {
            return Err(syn::Error::new_spanned(
                attr,
                "duplicate `poly_zipper_explicit` attribute",
            ));
        }
        let mut trait_set = BTreeSet::new();
        let mut saw_traits = false;
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("traits") {
                saw_traits = true;
                meta.parse_nested_meta(|meta| {
                    let ident = meta
                        .path
                        .get_ident()
                        .ok_or_else(|| meta.error("expected trait name identifier"))?;
                    let trait_kind = PolyZipperTrait::from_ident(ident).ok_or_else(|| {
                        meta.error("unknown trait name in poly_zipper_explicit traits list")
                    })?;
                    trait_set.insert(trait_kind);
                    Ok(())
                })
            } else {
                Err(meta.error("expected `traits(...)`"))
            }
        })?;
        if !saw_traits {
            return Err(syn::Error::new_spanned(attr, "expected `traits(...)`"));
        }
        if trait_set.is_empty() {
            return Err(syn::Error::new_spanned(attr, "traits list must not be empty"));
        }
        explicit = Some(trait_set);
    }
    Ok(explicit)
}

// See the docs for `PolyZipper` in `crate::pathmap::zipper::PolyZipper`
#[proc_macro_derive(PolyZipper)]
pub fn derive_poly_zipper(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_poly_zipper_with_traits(input, all_poly_zipper_traits(), true)
}

// See the docs for `PolyZipperExplicit` in `crate::pathmap::zipper::PolyZipperExplicit`
#[proc_macro_derive(PolyZipperExplicit, attributes(poly_zipper_explicit))]
pub fn derive_poly_zipper_explicit(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let explicit_traits = match parse_explicit_traits(&input.attrs) {
        Ok(value) => value,
        Err(err) => return TokenStream::from(err.to_compile_error()),
    };
    let mut traits = explicit_traits.unwrap_or_else(all_poly_zipper_traits);
    add_trait_dependencies(&mut traits);
    derive_poly_zipper_with_traits(input, traits, false)
}

fn derive_poly_zipper_with_traits(
    input: DeriveInput,
    traits: BTreeSet<PolyZipperTrait>,
    include_where_clause: bool,
) -> TokenStream {
    let enum_name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Extract enum variants
    let variants = match &input.data {
        Data::Enum(data_enum) => &data_enum.variants,
        _ => panic!("PolyZipper can only be derived for enums"),
    };

    // Generate From and TryFrom impls for each variant
    let from_impls = variants.iter().map(|variant| {
        let variant_name = &variant.ident;

        // Get the inner type (assuming single unnamed field)
        let inner_type = match &variant.fields {
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                &fields.unnamed[0].ty
            }
            _ => panic!("Each variant must have exactly one unnamed field"),
        };

        quote! {
            impl #impl_generics From<#inner_type> for #enum_name #ty_generics #where_clause {
                fn from(value: #inner_type) -> Self {
                    #enum_name::#variant_name(value)
                }
            }

            impl #impl_generics core::convert::TryFrom<#enum_name #ty_generics> for #inner_type #where_clause {
                type Error = ();

                fn try_from(value: #enum_name #ty_generics) -> Result<Self, Self::Error> {
                    match value {
                        #enum_name::#variant_name(inner) => Ok(inner),
                        _ => Err(()),
                    }
                }
            }
        }
    });

    let variant_arms: Vec<_> = variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! { Self::#variant_name(inner) }
        })
        .collect();

    let inner_types: Vec<_> = variants
        .iter()
        .map(|variant| match &variant.fields {
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => &fields.unnamed[0].ty,
            _ => panic!("Each variant must have exactly one unnamed field"),
        })
        .collect();

    // Generate Zipper trait implementation
    let zipper_impl = if traits.contains(&PolyZipperTrait::Zipper) {
        let variant_arms = &variant_arms;
        let zipper_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::Zipper,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::Zipper for #enum_name #ty_generics
            #zipper_where
            {
                fn path_exists(&self) -> bool {
                    match self {
                        #(#variant_arms => inner.path_exists(),)*
                    }
                }

                fn is_val(&self) -> bool {
                    match self {
                        #(#variant_arms => inner.is_val(),)*
                    }
                }

                fn child_count(&self) -> usize {
                    match self {
                        #(#variant_arms => inner.child_count(),)*
                    }
                }

                fn child_mask(&self) -> crate::pathmap::utils::ByteMask {
                    match self {
                        #(#variant_arms => inner.child_mask(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperValues trait implementation
    let zipper_values_impl = if traits.contains(&PolyZipperTrait::ZipperValues) {
        let variant_arms = &variant_arms;
        let zipper_values_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperValues<V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperValues<V> for #enum_name #ty_generics
            #zipper_values_where
            {
                fn val(&self) -> Option<&V> {
                    match self {
                        #(#variant_arms => inner.val(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperReadOnlyValues trait implementation
    let zipper_read_only_values_impl = if traits.contains(&PolyZipperTrait::ZipperReadOnlyValues) {
        let variant_arms = &variant_arms;
        let zipper_read_only_values_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperReadOnlyValues<'trie, V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperReadOnlyValues<'trie, V> for #enum_name #ty_generics
            #zipper_read_only_values_where
            {
                fn get_val(&self) -> Option<&'trie V> {
                    match self {
                        #(#variant_arms => inner.get_val(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate witness enum name and variant names for conditional traits
    let witness_enum_name = syn::Ident::new(&format!("{}Witness", enum_name), enum_name.span());
    let variant_names: Vec<_> = variants.iter().map(|variant| &variant.ident).collect();

    // Generate ZipperReadOnlyConditionalValues trait implementation with witness enum
    let zipper_read_only_conditional_values_impl = if traits
        .contains(&PolyZipperTrait::ZipperReadOnlyConditionalValues)
    {
        let zipper_read_only_conditional_values_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperReadOnlyConditionalValues<'trie, V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            //GOAT TODO: I think we could get into trouble if the witness types have fewer generics than
            // the outer PolyZipper enum.  So we probably should add a phantom case too.
            pub enum #witness_enum_name #impl_generics
            #zipper_read_only_conditional_values_where
            {
                #(#variant_names(<#inner_types as crate::pathmap::zipper::ZipperReadOnlyConditionalValues<'trie, V>>::WitnessT),)*
            }

            impl #impl_generics crate::pathmap::zipper::ZipperReadOnlyConditionalValues<'trie, V> for #enum_name #ty_generics
            #zipper_read_only_conditional_values_where
            {
                type WitnessT = #witness_enum_name #ty_generics;

                fn witness<'w>(&self) -> Self::WitnessT {
                    match self {
                        #(Self::#variant_names(inner) => #witness_enum_name::#variant_names(inner.witness()),)*
                    }
                }

                fn get_val_with_witness<'w>(&self, witness: &'w Self::WitnessT) -> Option<&'w V> where 'trie: 'w {
                    match (self, witness) {
                        #((Self::#variant_names(inner), #witness_enum_name::#variant_names(w)) => inner.get_val_with_witness(w),)*
                        _ => {
                            debug_assert!(false, "Witness variant must match zipper + variant");
                            None
                        },
                    }
                }
            }
        })
    } else {
        None
    };

    //GOAT, this is probably dead code given the decision (described in the PolyZipper docs)
    // not to support a `ZipperForking` impl
    //
    //I can't seem to figure out how to align the `'a` and the `'read_z` lifetimes without changing the trait definition,
    // and ideally we'd want to return a new enum-based zipper.  The elegant way to do that would be to actually invoke
    // the PolyZipper macro recursively to get maximum support on the new zipper, but there are a bunch of obnoxious details
    // to make that work. - like what to do about the recursion so we don't have infinite recursion in the macro, and how
    // to map the lifetimes required by the trait onto the lifetimes of the new zippers.
    //
    // // Generate ZipperForking trait implementation
    // let zipper_forking_impl = {
    //     let variant_arms = &variant_arms;
    //     let first_inner_type = &inner_types[0];
    //     let other_inner_types = &inner_types[1..];

    //     // Create modified generics with additional lifetime
    //     let mut forking_generics = generics.clone();
    //     forking_generics.params.insert(0, syn::parse_quote!('read_z));
    //     let (forking_impl_generics, _, _) = forking_generics.split_for_impl();

    //     quote! {
    //         impl #forking_impl_generics crate::pathmap::zipper::ZipperForking<V> for #enum_name #ty_generics
    //         where
    //             #(#inner_types: crate::pathmap::zipper::ZipperForking<V>,)*
    //             #(#other_inner_types: crate::pathmap::zipper::ZipperForking<V, ReadZipperT<'read_z> = <#first_inner_type as crate::pathmap::zipper::ZipperForking<V>>::ReadZipperT<'read_z>>,)*
    //             Self: 'read_z,
    //             #where_clause
    //         {
    //             type ReadZipperT<'a> = <#first_inner_type as crate::pathmap::zipper::ZipperForking<V>>::ReadZipperT<'a> where Self: 'a;

    //             fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
    //                 match self {
    //                     #(#variant_arms => inner.fork_read_zipper(),)*
    //                 }
    //             }
    //         }
    //     }
    // };

    // Generate ZipperMoving trait implementation
    let zipper_moving_impl = if traits.contains(&PolyZipperTrait::ZipperMoving) {
        let variant_arms = &variant_arms;
        let zipper_moving_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperMoving,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperMoving for #enum_name #ty_generics
            #zipper_moving_where
            {
                fn path(&self) -> &[u8] {
                    match self {
                        #(#variant_arms => inner.path(),)*
                    }
                }

                fn val_count(&self) -> usize {
                    match self {
                        #(#variant_arms => inner.val_count(),)*
                    }
                }

                fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) {
                    match self {
                        #(#variant_arms => inner.descend_to(k),)*
                    }
                }

                fn descend_to_check<K: AsRef<[u8]>>(&mut self, k: K) -> bool {
                    match self {
                        #(#variant_arms => inner.descend_to_check(k),)*
                    }
                }

                #[inline]
                fn descend_to_byte(&mut self, k: u8) {
                    match self {
                        #(#variant_arms => inner.descend_to_byte(k),)*
                    }
                }

                #[inline]
                fn descend_to_existing_byte(&mut self, k: u8) -> bool {
                    match self {
                        #(#variant_arms => inner.descend_to_existing_byte(k),)*
                    }
                }

                fn ascend(&mut self, steps: usize) -> bool {
                    match self {
                        #(#variant_arms => inner.ascend(steps),)*
                    }
                }

                fn ascend_until(&mut self) -> bool {
                    match self {
                        #(#variant_arms => inner.ascend_until(),)*
                    }
                }

                fn ascend_until_branch(&mut self) -> bool {
                    match self {
                        #(#variant_arms => inner.ascend_until_branch(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperConcrete trait implementation
    let zipper_concrete_impl = if traits.contains(&PolyZipperTrait::ZipperConcrete) {
        let variant_arms = &variant_arms;
        let zipper_concrete_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperConcrete,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperConcrete for #enum_name #ty_generics
            #zipper_concrete_where
            {
                fn shared_node_id(&self) -> Option<u64> {
                    match self {
                        #(#variant_arms => inner.shared_node_id(),)*
                    }
                }
                fn is_shared(&self) -> bool {
                    match self {
                        #(#variant_arms => inner.is_shared(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperAbsolutePath trait implementation
    let zipper_absolute_path_impl = if traits.contains(&PolyZipperTrait::ZipperAbsolutePath) {
        let variant_arms = &variant_arms;
        let zipper_absolute_path_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperAbsolutePath,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperAbsolutePath for #enum_name #ty_generics
            #zipper_absolute_path_where
            {
                fn origin_path(&self) -> &[u8] {
                    match self {
                        #(#variant_arms => inner.origin_path(),)*
                    }
                }

                fn root_prefix_path(&self) -> &[u8] {
                    match self {
                        #(#variant_arms => inner.root_prefix_path(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperPathBuffer trait implementation
    let zipper_path_buffer_impl = if traits.contains(&PolyZipperTrait::ZipperPathBuffer) {
        let variant_arms = &variant_arms;
        let zipper_path_buffer_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperPathBuffer,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperPathBuffer for #enum_name #ty_generics
            #zipper_path_buffer_where
            {
                unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
                    match self {
                        #(#variant_arms => unsafe { inner.origin_path_assert_len(len) },)*
                    }
                }

                fn prepare_buffers(&mut self) {
                    match self {
                        #(#variant_arms => inner.prepare_buffers(),)*
                    }
                }

                fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) {
                    match self {
                        #(#variant_arms => inner.reserve_buffers(path_len, stack_depth),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperIteration trait implementation
    let zipper_iteration_impl = if traits.contains(&PolyZipperTrait::ZipperIteration) {
        let variant_arms = &variant_arms;
        let zipper_iteration_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperIteration,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperIteration for #enum_name #ty_generics
            #zipper_iteration_where
            {
                fn to_next_val(&mut self) -> bool {
                    match self {
                        #(#variant_arms => inner.to_next_val(),)*
                    }
                }

                fn descend_last_path(&mut self) -> bool {
                    match self {
                        #(#variant_arms => inner.descend_last_path(),)*
                    }
                }

                fn descend_first_k_path(&mut self, k: usize) -> bool {
                    match self {
                        #(#variant_arms => inner.descend_first_k_path(k),)*
                    }
                }

                fn to_next_k_path(&mut self, k: usize) -> bool {
                    match self {
                        #(#variant_arms => inner.to_next_k_path(k),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperReadOnlyIteration trait implementation
    let zipper_read_only_iteration_impl = if traits
        .contains(&PolyZipperTrait::ZipperReadOnlyIteration)
    {
        let variant_arms = &variant_arms;
        let zipper_read_only_iteration_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperReadOnlyIteration<'trie, V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperReadOnlyIteration<'trie, V> for #enum_name #ty_generics
            #zipper_read_only_iteration_where
            {
                fn to_next_get_val(&mut self) -> Option<&'trie V> {
                    match self {
                        #(#variant_arms => inner.to_next_get_val(),)*
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperReadOnlyConditionalIteration trait implementation
    let zipper_read_only_conditional_iteration_impl = if traits
        .contains(&PolyZipperTrait::ZipperReadOnlyConditionalIteration)
    {
        let zipper_read_only_conditional_iteration_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperReadOnlyConditionalIteration<'trie, V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperReadOnlyConditionalIteration<'trie, V> for #enum_name #ty_generics
            #zipper_read_only_conditional_iteration_where
            {
                fn to_next_get_val_with_witness<'w>(&mut self, witness: &'w Self::WitnessT) -> Option<&'w V> where 'trie: 'w {
                    match (self, witness) {
                        #((Self::#variant_names(inner), #witness_enum_name::#variant_names(w)) => inner.to_next_get_val_with_witness(w),)*
                        _ => {
                            debug_assert!(false, "Witness variant must match zipper variant");
                            None
                        },
                    }
                }
            }
        })
    } else {
        None
    };

    // Generate ZipperSubtries trait implementation
    //GOAT, TODO: We ought to use the generic allocator A, if the enum has one, but use the GlobalAlloc if not,
    // this requires parsing the impl_generics to see if there is an `A: Allocator` that is defined
    let zipper_subtries_impl = if traits.contains(&PolyZipperTrait::ZipperSubtries) {
        let zipper_subtries_where = if include_where_clause {
            quote! {
                where
                    #(#inner_types: crate::pathmap::zipper::ZipperSubtries<V>,)*
                    #where_clause
            }
        } else {
            quote! {}
        };
        Some(quote! {
            impl #impl_generics crate::pathmap::zipper::ZipperSubtries<V> for #enum_name #ty_generics
            #zipper_subtries_where
            {
                fn native_subtries(&self) -> bool {
                    match self {
                        #(#variant_arms => inner.native_subtries(),)*
                    }
                }
                fn try_make_map(&self) -> Option<crate::pathmap::PathMap<V>> {
                    match self {
                        #(#variant_arms => inner.try_make_map(),)*
                    }
                }
                fn trie_ref(&self) -> Option<crate::pathmap::zipper::TrieRef<'_, V>> {
                    match self {
                        #(#variant_arms => inner.trie_ref(),)*
                    }
                }
                fn alloc(&self) -> crate::pathmap::alloc::GlobalAlloc {
                    crate::pathmap::alloc::global_alloc()
                }
            }
        })
    } else {
        None
    };

    let expanded = quote! {
        #(#from_impls)*
        #zipper_impl
        #zipper_values_impl
        #zipper_read_only_values_impl
        #zipper_read_only_conditional_values_impl
        // #zipper_forking_impl
        #zipper_moving_impl
        #zipper_concrete_impl
        #zipper_absolute_path_impl
        #zipper_path_buffer_impl
        #zipper_iteration_impl
        #zipper_read_only_iteration_impl
        #zipper_read_only_conditional_iteration_impl
        #zipper_subtries_impl
    };

    TokenStream::from(expanded)
}

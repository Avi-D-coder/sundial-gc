use crate::{
    arena::Arena,
    gc::Gc,
    mark::*,
};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::{mem, ptr};

/// TODO support other hashes, FxHash is not secure.

/// An Immutable unordered KV map.
/// A HAMT (hash array mapped trie), also known as a ideal hash tree or simply immutable hashmap.
/// Technically this is a modified CHAMP, but the differences between this a CHAMP and a HAMT are minor.
pub struct HashMap<'r, K, V> {
    /// Bit map 1 denotes a value or child map.
    /// bit 16 denotes a collision node
    entries: u16,
    /// Bit map 1 denotes a value.
    has_val: u16,
    arr: [*const (); 15],
    // TODO have another go at using a union
    kv: PhantomData<Gc<'r, (&'r K, &'r V)>>,
}

impl<'r, K, V> Copy for HashMap<'r, K, V> {}

impl<'r, K, V> Clone for HashMap<'r, K, V> {
    fn clone(&self) -> Self {
        *self
    }
}

#[test]
fn size_of_hashmap() {
    assert_eq!(mem::size_of::<HashMap<usize, usize>>(), 128);
}

impl<'r, K, V> Default for HashMap<'r, K, V> {
    fn default() -> Self {
        HashMap {
            entries: 0,
            has_val: 0,
            arr: [ptr::null(); 15],
            kv: PhantomData,
        }
    }
}

impl<'r, K: Hash + Eq + Clone + Condemned, V: Clone + Condemned> HashMap<'r, K, V> {
    pub fn get(&'r self, key: &K) -> Option<&'r V> {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();

        unsafe { self.get_hashed(key, hash, 0) }
    }

    /// Hash must be created by FxHash::default and left shifted on_bit times
    pub unsafe fn get_hashed(&'r self, key: &K, hash: u64, on_bit: u8) -> Option<&'r V> {
        if on_bit == 64 {
            panic!("TODO handle collisions")
        }

        // FIXME  32 bit systems
        // FIXME  Big endian bit systems
        let slot = (0b1111 & hash) as u8;
        let slot = slot.saturating_sub(1);
        let mask = 1 << slot;

        if (self.entries & mask) == 0 {
            None
        } else if (self.has_val & mask) == 0 {
            let Gc((k, v), ..) = mem::transmute(self.arr[slot as usize]);
            if *key == *k {
                Some(v)
            } else {
                None
            }
        } else {
            // Slot is a child HashMap
            let child: &'r Self = mem::transmute(self.arr[slot as usize]);
            child.get_hashed(key, hash >> 4, on_bit + 4)
        }
    }

    pub fn set<'a, 'b>(
        self: Gc<'r, HashMap<'r, K, V>>,
        key: K,
        value: V,
        arena_kv: &'a Arena<(K, V)>,
        arena_hm: &'b Arena<Self>,
    ) -> Gc<'r, HashMap<'r, K, V>> {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();

        unsafe { self.set_hashed(key, hash, 0, value, arena_kv, arena_hm) }
    }

    #[inline(always)]
    pub unsafe fn set_hashed<'a, 'b>(
        self: Gc<'r, HashMap<'r, K, V>>,
        key: K,
        hash: u64,
        on_bit: u8,
        value: V,
        arena_kv: &'a Arena<(K, V)>,
        arena_hm: &'b Arena<Self>,
    ) -> Gc<'r, HashMap<'r, K, V>> {
        let mut spine = arena_hm.box_copy(&*self);

        // FIXME  32 bit systems
        // FIXME  Big endian bit systems
        let slot = (0b1111 & hash) as u8;
        let slot = slot.saturating_sub(1);
        let mask = 1 << slot;

        if (self.entries & mask) == 0 {
            spine.entries |= mask;
            spine.has_val |= mask;

            spine.arr[slot as usize] =
                arena_kv.gc_alloc((key, value)).0 as *const (K, V) as *const ();
            Gc::from(spine)
        } else if (self.has_val & mask) != 0 {
            let pre_kv @ Gc((pre_key, _), _): Gc<(K, V)> = mem::transmute(self.arr[slot as usize]);
            if key == *pre_key {
                let new_kv = arena_kv.gc_alloc((key, value));
                spine.arr[slot as usize] = new_kv.0 as *const (K, V) as *const ();

                Gc::from(spine)
            } else {
                // spine.entries |= mask; // already set
                spine.has_val ^= mask;

                let new_kv = arena_kv.gc_alloc((key, value));

                let mut hasher = FxHasher::default();
                pre_key.hash(&mut hasher);
                let pre_hash = hasher.finish();

                let on_bit = on_bit + 4;

                let mut empty = arena_hm.box_copy(&HashMap::default());
                spine.arr[slot as usize] = empty.0 as *const Self as *const ();

                empty.set_collied(
                    hash >> 4,
                    pre_hash >> on_bit,
                    new_kv,
                    pre_kv,
                    on_bit,
                    arena_kv,
                    arena_hm,
                );

                Gc::from(spine)
            }
        } else {
            // Slot is a child HashMap
            let child: Gc<'r, HashMap<'r, K, V>> = mem::transmute(self.arr[slot as usize]);
            child.set_hashed(key, hash >> 4, on_bit + 4, value, arena_kv, arena_hm)
        }
    }

    unsafe fn set_collied(
        &mut self,
        hash1: u64,
        hash2: u64,
        kv1: Gc<'r, (K, V)>,
        kv2: Gc<'r, (K, V)>,
        on_bit: u8,
        arena_kv: &Arena<(K, V)>,
        arena_hm: &Arena<Self>,
    ) {
        if on_bit == 64 {
            panic!("TODO handle collisions")
        }

        let slot1 = (0b1111 & hash1) as u8;
        let slot1 = slot1.saturating_sub(1);

        let slot2 = (0b1111 & hash2) as u8;
        let slot2 = slot2.saturating_sub(1);

        if slot1 == slot2 {
            let mut empty = arena_hm.box_copy(&HashMap::default());
            self.arr[slot1 as usize] = empty.0 as *const Self as *const ();

            empty.set_collied(
                hash1 >> 4,
                hash2 >> 4,
                kv1,
                kv2,
                on_bit + 4,
                arena_kv,
                arena_hm,
            );
        } else {
            // No collision this time!
            let mask1 = 1 << slot1;
            let mask2 = 1 << slot2;
            self.entries |= mask1;
            self.entries |= mask2;
            self.has_val |= mask1;
            self.has_val |= mask2;

            self.arr[slot1 as usize] = kv1.0 as *const (K, V) as *const ();
            self.arr[slot2 as usize] = kv2.0 as *const (K, V) as *const ();
        }
    }
}

#[test]
fn hamt_get_set_test() {
    let arena_hm = &Arena::<HashMap<usize, usize>>::new();
    let arena_kv = &Arena::<(usize, usize)>::new();

    let empty = arena_hm.gc_alloc(HashMap::default());
    let one = empty.set(1, 1, arena_kv, arena_hm);
    // FIXME this paniced once. There's a use after free somewhere.
    assert_eq!(*one.get(&1).unwrap(), 1);
}

// There is no way to derive Condemed since HashMap uses unsafe casts
unsafe impl<'r, K: Condemned + 'r, V: Condemned> Condemned for HashMap<'r, K, V> {
    default fn feilds(x: &Self, offset: u8, grey_feilds: u8, invariant: &Invariant) -> u8 {
        assert!(Self::PRE_CONDTION);
        let mut bloom = 0b0000000;

        for i in 0..16 {
            let mask = 1 << i;
            let has_val = (x.has_val & mask) != 0;
            let sub_map = (x.entries & mask) != 0;

            if has_val {
                let kv: Gc<(K, V)> = unsafe { mem::transmute(x.arr[i]) };
                bloom |= Condemned::feilds(&kv, offset, grey_feilds, invariant)
            } else if sub_map {
                let hm: Gc<HashMap<K, V>> = unsafe { mem::transmute(x.arr[i]) };
                bloom |= Condemned::feilds(&hm, offset + 1, grey_feilds, invariant)
            };
        }
        bloom
    }

    default unsafe fn evacuate<'e>(
        x: &Self,
        offset: u8,
        grey_feilds: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        for i in 0..16 {
            let mask = 1 << i;
            let has_val = (x.has_val & mask) != 0;
            let sub_map = (x.entries & mask) != 0;

            if has_val {
                let kv: Gc<(K, V)> = mem::transmute(x.arr[i]);
                Condemned::evacuate(&kv, offset, grey_feilds, invariant, handlers);
            } else if sub_map {
                let hm: Gc<HashMap<K, V>> = mem::transmute(x.arr[i]);
                Condemned::evacuate(&hm, offset + 1, grey_feilds, invariant, handlers);
            };
        }
    }

    default fn direct_gc_types(t: &mut std::collections::HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        Gc::<(K, V)>::direct_gc_types(t, offset);
        Gc::<HashMap<K, V>>::direct_gc_types(t, offset + 1);
    }

    default fn transitive_gc_types(tti: *mut Tti) {
        Gc::<HashMap<K, V>>::transitive_gc_types(tti);
        Gc::<(K, V)>::transitive_gc_types(tti);
    }

    /// 2
    default const GC_COUNT: u8 = Gc::<(K, V)>::GC_COUNT + Gc::<HashMap<K, V>>::GC_COUNT;
    default const PRE_CONDTION: bool = if K::PRE_CONDTION && V::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    };
}

#[test]
fn direct_gc_types_hamt_test() {
    let mut dgt = std::collections::HashMap::with_capacity(20);
    HashMap::<usize, usize>::direct_gc_types(&mut dgt, 0);
    let tys: Vec<_> = dgt.into_iter().map(|(i, _)| i).collect();
    assert_eq!(tys.len(), 2)
}

#[test]
fn gc_count_hamt_test() {
    assert_eq!(2, HashMap::<usize, usize>::GC_COUNT);
    assert_eq!(2, HashMap::<usize, Gc<(usize, usize)>>::GC_COUNT);
}

#[test]
fn translator_hamt_test() {
    let mut dgt = std::collections::HashMap::with_capacity(20);
    HashMap::<usize, usize>::direct_gc_types(&mut dgt, 0);
    let t = crate::mark::Translator::from::<HashMap<usize, usize>>(
        &dgt.into_iter().map(|(ti, _)| ti).collect(),
    );
    assert_ne!(t.0[0], 255);
    assert_ne!(t.0[1], 255);
}

// /// TODO A HAMT, specialized to untagged ptrs.
// pub struct HashMapPtrs<T> {
//     /// A tagged ptr to HashMap or .
//     arr: [*const (); 16],
//     t: PhantomData<T>,
// }

use crate::{arena::Arena, gc::Gc, mark::Condemned};
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

    pub fn set(
        &'r self,
        key: K,
        value: V,
        arena_kv: &Arena<(K, V)>,
        arena_hm: &Arena<Self>,
    ) -> Gc<'r, HashMap<'r, K, V>> {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();

        unsafe { self.set_hashed(key, hash, 0, value, arena_kv, arena_hm) }
    }

    #[inline(always)]
    pub unsafe fn set_hashed(
        &'r self,
        key: K,
        hash: u64,
        on_bit: u8,
        value: V,
        arena_kv: &Arena<(K, V)>,
        arena_hm: &Arena<Self>,
    ) -> Gc<'r, HashMap<'r, K, V>> {
        let mut spine = arena_hm.box_copy(self);

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
            let child: &'r Self = mem::transmute(self.arr[slot as usize]);
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

// /// TODO A HAMT, specialized to untagged ptrs.
// pub struct HashMapPtrs<T> {
//     /// A tagged ptr to HashMap or .
//     arr: [*const (); 16],
//     t: PhantomData<T>,
// }

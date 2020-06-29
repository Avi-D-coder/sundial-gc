use crate::{arena::Arena, gc::Gc, mark::Condemned};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, size_of};
use std::ops::Deref;

/// TODO support other hashes, FxHash is not secure.

/// An Immutable unordered KV map.
/// A HAMT (hash array mapped trie), also known as a ideal hash tree or simply immutable hashmap.
/// Technically this is a modified CHAMP, but the differences between this a CHAMP and a HAMT are minor.
#[derive(Copy, Clone)]
pub struct HashMap<'r, K, V> {
    /// Bit map 1 denotes a value or child map.
    /// bit 16 denotes a collision node
    empty: u16,
    /// Bit map 1 denotes a value.
    is_val: u16,
    arr: [*const (); 15],
    kv: PhantomData<Gc<'r, (&'r K, &'r V)>>,
}

#[test]
fn size_of_hashmap() {
    assert_eq!(size_of::<HashMap<usize, usize>>(), 128);
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

        if (self.empty & mask) != 0 {
            None
        } else if (self.is_val & mask) == 0 {
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
        arena_kv: Arena<(K, V)>,
        arena_hm: Arena<Self>,
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
        arena_kv: Arena<(K, V)>,
        arena_hm: Arena<Self>,
    ) -> Gc<'r, HashMap<'r, K, V>> {
        todo!()
        //     arena_hm.gc_alloc()
        //     // FIXME  32 bit systems
        //     // FIXME  Big endian bit systems
        //     let slot = (0b1111 & hash) as u8;
        //     let slot = slot.saturating_sub(1);
        //     let mask = 1 << slot;

        //     if (self.empty & mask) != 0 {
        //         None
        //     } else if (self.is_val & mask) == 0 {
        //         let Gc((k, v)) = mem::transmute(self.arr[slot as usize]);
        //         if *key == *k {
        //             Some(v)
        //         } else {
        //             None
        //         }
        //     } else {
        //         // Slot is a child HashMap
        //         let child: &'r Self = mem::transmute(self.arr[slot as usize]);
        //         child.get_hashed(key, hash >> 4, on_bit + 4)
        //     }
    }
}

// /// TODO A HAMT, specialized to untagged ptrs.
// pub struct HashMapPtrs<T> {
//     /// A tagged ptr to HashMap or .
//     arr: [*const (); 16],
//     t: PhantomData<T>,
// }

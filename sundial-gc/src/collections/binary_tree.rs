use crate::{Gc, Trace};

pub enum BinaryTree<'r, K: 'r, V: 'r> {
    Empty,
    Branch(Gc<'r, (K, Self, Self, V)>),
}

impl<'r, K: Ord, V> BinaryTree<'r, K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        match self {
            BinaryTree::Empty => None,
            BinaryTree::Branch(Gc((k, l, r, v), ..)) => match key.cmp(k) {
                std::cmp::Ordering::Equal => Some(v),
                std::cmp::Ordering::Less => l.get(key),
                std::cmp::Ordering::Greater => r.get(key),
            },
        }
    }
}

unsafe impl<'r, K: 'r, V: 'r> Trace for BinaryTree<'r, K, V> {
    fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &crate::mark::Invariant) -> u8 {
        let mut bloom = 0b0000000;
        match s {
            BinaryTree::Empty => (),
            BinaryTree::Branch(gc) => bloom |= Trace::fields(gc, offset, grey_fields, invariant),
        }
        bloom
    }
    unsafe fn evacuate<'e>(
        s: &Self,
        offset: crate::mark::Offset,
        grey_fields: u8,
        invariant: &crate::mark::Invariant,
        handlers: &mut crate::mark::Handlers,
    ) {
        match s {
            BinaryTree::Empty => (),
            BinaryTree::Branch(gc) => {
                Gc::<'r, (K, Self, Self, V)>::evacuate(gc, offset, grey_fields, invariant, handlers)
            }
        }
    }
    fn direct_gc_types(
        t: &mut std::collections::HashMap<crate::mark::GcTypeInfo, crate::mark::TypeRow>,
        offset: u8,
    ) {
        Gc::<'r, (K, Self, Self, V)>::direct_gc_types(t, offset)
    }
    fn transitive_gc_types(tti: *mut crate::mark::Tti) {
        Gc::<'r, (K, Self, Self, V)>::transitive_gc_types(tti)
    }
    const GC_COUNT: u8 = Gc::<'r, (K, Self, Self, V)>::GC_COUNT;
    const PRE_CONDTION: bool = Gc::<'r, (K, Self, Self, V)>::PRE_CONDTION;
}

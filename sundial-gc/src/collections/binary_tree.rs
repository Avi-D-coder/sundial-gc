use crate as sundial_gc;
use std::cmp::Ordering::*;
use sundial_gc::*;
use sundial_gc_derive::*;

#[derive(Debug, Copy, Clone, Trace)]
pub enum BinaryTree<'r, K, V>
where
    K: 'r,
    V: 'r,
{
    Empty,
    Branch(Gc<'r, (K, Self, Self, V)>),
}

impl<'r, K: Ord, V> BinaryTree<'r, K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        match self {
            BinaryTree::Empty => None,
            BinaryTree::Branch(Gc((k, l, r, v), ..)) => match key.cmp(k) {
                Equal => Some(v),
                Less => l.get(key),
                Greater => r.get(key),
            },
        }
    }
}

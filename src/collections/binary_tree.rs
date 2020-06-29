use crate::gc::Gc;

pub enum BinaryTree<'r, K, V> {
    Empty,
    Branch(Gc<'r, (K, Self, Self, V)>),
}

impl<'r, K: Ord, V> BinaryTree<'r, K, V> {
    fn get(&self, key: &K) -> Option<&V> {
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

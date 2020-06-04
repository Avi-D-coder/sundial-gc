use super::Gc;

pub unsafe trait Mark<'o, 'n, O, N> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'n, N>;
}

// GAT Mark
// pub unsafe trait Mark<'o, 'n, O> {
//     type Struct<'l>;
//     fn mark(&'n self, o: Gc<'o, Self::Struct<'o>>) -> Gc<'n, Self::Struct<'n>>;
// }

// Blanket Arena<T> impl is in src/arena.rs

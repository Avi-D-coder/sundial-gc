#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(generic_associated_types)]

use sundial_gc::*;
use sundial_gc_derive::*;

#[derive(Trace)]
pub enum BinaryTree<'r, K, V>
where
    K: 'r,
    V: 'r,
{
    Empty,
    Branch(Gc<'r, (K, Self, Self, V)>),
}

#[derive(Trace)]
struct Foo<T> {
    t: T,
}

#[allow(dead_code)]
#[derive(Trace)]
struct Bar<B>(B, i8);

#[derive(Trace)]
struct Usize(usize);

#[allow(dead_code)]
#[derive(Trace)]
enum FooBarBazz<A, B> {
    Foo { f: A, b: usize },
    Bar(B),
    Bazz,
}

#[derive(Trace)]
struct List<'r, T>
where
    T: 'r,
{
    t: T,
    next: Option<Gc<'r, List<'r, T>>>,
}

#[test]
fn derive_trace_test() {}

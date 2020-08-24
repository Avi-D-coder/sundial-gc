#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(generic_associated_types)]
#![allow(dead_code)]

use collections::List;
use sundial_gc::*;
use sundial_gc_derive::*;

fn main() {}

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Expr<'r> {
    Var(usize),
    Lam(Gc<'r, Expr<'r>>),
    App(Gc<'r, Expr<'r>>, Gc<'r, Expr<'r>>),
    Lit(isize),
    Prim(Op, Gc<'r, Expr<'r>>, Gc<'r, Expr<'r>>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Op {
    Add,
    Mul,
}

impl Op {
    fn eval<'r>(self, a: Value<'r>, b: Value<'r>) -> Value<'r> {
        match (self, a, b) {
            (Op::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Op::Mul, Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            _ => panic!(),
        }
    }
}

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Value<'r> {
    Int(isize),
    Closure(Gc<'r, Expr<'r>>, Env<'r>),
}

type Env<'r> = List<'r, Value<'r>>;

fn eval<'r>(env: Env<'r>, term: Gc<'r, Expr<'r>>) -> Value<'r> {
    match *term {
        Expr::Var(n) => env[n],
        Expr::Lam(a) => Value::Closure(a, env),
        Expr::App(a, b) => {
            let (c, env_) = match eval(env, a) {
                Value::Closure(c, env_) => (c, env_),
                _ => panic!(),
            };
            let v = eval(env, b);
            eval(env_.cons(v, &Arena::new()), c)
        }
        Expr::Lit(n) => Value::Int(n),
        Expr::Prim(op, a, b) => op.eval(eval(env, a), eval(env, b)),
    }
}

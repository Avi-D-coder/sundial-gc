#![allow(incomplete_features)]
#![feature(specialization)]

use collections::Map;
use sundial_gc::*;
use sundial_gc_derive::*;

fn main() {}

// #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord,)]
// enum Tokens {
//     Text,
//     Bracket,
//     Operator
// }

// fn parse<'r>(expr: &str, arena: &Arena<Expr<'r>>) -> Result<Expr<'r>, String> {
//     expr.chars().filter(|c| !c.is_whitespace()).group_by();
//     todo!()
// }

mod parse {
    use crate::{Expr, GcExpr, Ident, Ty};
    use nom::{
        branch,
        bytes::complete::is_not,
        // see the "streaming/complete" paragraph lower for an explanation of these submodules
        character::complete::{alphanumeric1, char, digit1, space1},
        do_parse,
        map,
        named,
        named_args,
        sequence::delimited,
        tag,
        IResult,
    };
    use sundial_gc::Arena;

    fn parens(input: &str) -> IResult<&str, &str> {
        delimited(char('('), is_not(")"), char(')'))(input)
    }

    fn ty<'a, 'r>(arena: &'r Arena<String>, input: &'a str) -> IResult<&'a str, Ty<'r>> {
        let string = |i| {
            map!(i, delimited(char('"'), is_not("\""), char('"')), |s| {
                Ty::Str(arena.gc(String::from(s)))
            })
        };

        let int = |i| map!(i, digit1, |i: &str| Ty::Int(i.parse().unwrap()));

        let tru = |i| map!(i, tag!("True"), |_| Ty::Bool(true));
        let fal = |i| map!(i, tag!("False"), |_| Ty::Bool(false));

        branch::alt((int, tru, fal, string))(input)
    }

    fn ident<'a, 'r>(input: &'a str, arena: &'r Arena<String>) -> IResult<&'a str, Ident<'r>> {
        map!(input, alphanumeric1, |i| arena.gc(String::from(i)))
    }

    fn bind<'a, 'r>(
        input: &'a str,
        strs: &'r Arena<String>,
        exprs: &'r Arena<Expr<'r>>,
    ) -> IResult<&'a str, GcExpr<'r>> {
        let ident = |i| ident(i, strs);

        let expr = |i| expr(i, strs, exprs);
        do_parse!(
            input,
            tag!("let") >> space1 >> n: ident >> space1 >> e: expr >> (exprs.gc(Expr::Bind(n, e)))
        )
    }

    fn lam<'a, 'r>(
        input: &'a str,
        strs: &'r Arena<String>,
        exprs: &'r Arena<Expr<'r>>,
    ) -> IResult<&'a str, GcExpr<'r>> {
        let expr = |i| expr(i, strs, exprs);

        do_parse!(
            input,
            tag!("\\") >> space1 >> parens >> space1 >> e: expr >> (exprs.gc(Expr::Lam(e)))
        )
    }

    fn app<'a, 'r>(
        input: &'a str,
        strs: &'r Arena<String>,
        exprs: &'r Arena<Expr<'r>>,
    ) -> IResult<&'a str, GcExpr<'r>> {
        let expr = |i| expr(i, strs, exprs);

        let args = do_parse!(input, e: expr >> (parens));

        // To curry or not to curry that is the question.
        todo!()
    }

    fn expr<'a, 'r>(
        input: &'a str,
        strs: &'r Arena<String>,
        exprs: &'r Arena<Expr<'r>>,
    ) -> IResult<&'a str, GcExpr<'r>> {
        let lit = |i| map!(i, |i| ty(strs, i), |ty| exprs.gc(Expr::Lit(ty)));

        let var = |i| map!(i, |i| ident(i, strs), |n| exprs.gc(Expr::Var(n)));
        let exp = |i| expr(i, strs, exprs);
        let lam = |i| lam(i, strs, exprs);
        let bind = |i| bind(i, strs, exprs);
        let app = |i| app(i, strs, exprs);

        branch::alt((lit, bind, lam, var, app))(input)
    }
}

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Expr<'r> {
    Lit(Ty<'r>),
    Var(Ident<'r>),
    Lam(GcExpr<'r>),
    App(GcExpr<'r>, Map<'r, Ident<'r>, GcExpr<'r>>),
    Prim(Op, GcExpr<'r>, GcExpr<'r>),
    Bind(Ident<'r>, GcExpr<'r>),
}

type GcExpr<'r> = Gc<'r, Expr<'r>>;
type Ident<'r> = Gc<'r, String>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Op {
    Add,
    Mul,
    Eql,
}

impl Op {
    fn eval<'r>(self, a: Value<'r>, b: Value<'r>) -> Value<'r> {
        match (self, a, b) {
            (Op::Add, Type(Int(a)), Type(Int(b))) => Type(Int(a + b)),
            (Op::Mul, Type(Int(a)), Type(Int(b))) => Type(Int(a * b)),
            (Op::Eql, Type(a), Type(b)) => Type(Bool(a == b)),
            _ => panic!("Hit unevaluated closure"),
        }
    }
}

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Value<'r> {
    Type(Ty<'r>),
    Closure(GcExpr<'r>, Env<'r>),
}
use Value::*;

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Ty<'r> {
    Bool(bool),
    Int(isize),
    Str(Gc<'r, String>),
}
use Ty::*;

type Env<'r> = Map<'r, Gc<'r, String>, Value<'r>>;

fn eval<'r>(env: Env<'r>, term: Gc<'r, Expr<'r>>) -> Value<'r> {
    match *term {
        Expr::Lit(ty) => Type(ty),
        Expr::Var(n) => env[&n],
        Expr::Lam(a) => Value::Closure(a, env),
        Expr::App(l, args) => match eval(env, l) {
            Type(ty) => panic!("{:?} is not a function", ty),
            Closure(expr, captured_env) => {
                let args: Map<_, _> = args
                    .iter()
                    .map(|(parm, arg)| (*parm, eval(env, *arg)))
                    .collect();
                // Arguments take precedence over variables in scope.
                let env = args.union(captured_env, &Arena::new());

                eval(env, expr)
            }
        },
        Expr::Prim(op, a, b) => op.eval(eval(env, a), eval(env, b)),
        Expr::Bind(name, expr) => eval(env.insert(name, eval(env, expr), &Arena::new()), term),
    }
}

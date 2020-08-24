#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(generic_associated_types)]

use rustyline::Editor;
use sundial_gc::{collections::Map, *};
use sundial_gc_derive::*;

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();

    let mut partial = String::default();
    let mut global = Env::default();
    loop {
        // let readline = rl.readline("λ ");
        // match readline {
        //     Ok(line) => {
        //         rl.add_history_entry(&line);
        //         partial.extend(line.chars());

        //         let strs = Arena::new();
        //         let nodes = Arena::new();
        //         let exprs = Arena::new();
        //         if let Ok((_, term)) = parse::term(&partial, &strs, &exprs, &nodes) {
        //             eprintln!("{:?}", term);
        //             let vals = Arena::new();
        //             match term {
        //                 Term::Fun(name, expr) => {
        //                     // global = global.insert(name, eval(global, expr, &vals), &Arena::new());
        //                 }
        //                 Term::Exp(expr) => {
        //                     println!("{:?}", eval(global, expr, &vals));
        //                 }
        //             };
        //             partial.clear();
        //         };
        //     }
        //     Err(_) => {
        //         break;
        //     }
        // }
        // global.iter().for_each(|kv|  eprintln!("{:?}", kv));
    }
}

mod parse {
    use crate::{Expr, GcExpr, Ident, Op, Term, Ty};
    use nom::{
        branch,
        bytes::streaming::is_not,
        character::complete::{alphanumeric1, char, digit1, space0, space1},
        do_parse,
        error::ErrorKind,
        map,
        sequence::delimited,
        tag, IResult,
    };
    use sundial_gc::{
        collections::{Map, Node},
        Arena,
    };

    fn parens(input: &str) -> IResult<&str, &str> {
        delimited(char('('), is_not(")"), char(')'))(input)
    }

    fn ty<'l, 'r, 'a: 'r>(arena: &'a Arena<String>, input: &'l str) -> IResult<&'l str, Ty<'r>> {
        dbg!(input);
        let string = |i| {
            map!(i, delimited(char('"'), is_not("\""), char('"')), |s| {
                Ty::Str(arena.gc(String::from(s)))
            })
        };

        let int = |i: &'l str| do_parse!(i, d: digit1 >> (Ty::Int(d.parse().unwrap())));

        let tru = |i: &'l str| do_parse!(i, tag!("True") >> (Ty::Bool(true)));
        let fal = |i: &'l str| do_parse!(i, tag!("False") >> (Ty::Bool(false)));

        dbg!(branch::alt((int, tru, fal, string))(input))
    }

    #[test]
    fn ty_test() {
        assert_eq!(ty(&Arena::new(), "1"), Ok(("", Ty::Int(1))));
        assert_eq!(ty(&Arena::new(), "True"), Ok(("", Ty::Bool(true))));
        assert_eq!(ty(&Arena::new(), "False"), Ok(("", Ty::Bool(false))));
        assert_eq!(
            ty(&Arena::new(), "\"Foo\""),
            Ok(("", Ty::Str(Arena::new().gc(String::from("Foo")))))
        );
    }

    fn ident<'l, 'r, 'a: 'r>(
        input: &'l str,
        arena: &'a Arena<String>,
    ) -> IResult<&'l str, Ident<'r>> {
        dbg!(input);
        // Identifiers must start with a letter.
        if let Some(c) = input.chars().next() {
            if !c.is_lowercase() {
                return Err(nom::Err::Error(("Not a identifier", ErrorKind::NoneOf)));
            };
        };

        dbg!(map!(input, alphanumeric1, |i| arena.gc(String::from(i))))
    }

    fn bind<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, GcExpr<'r>> {
        dbg!(input);
        let ident = |i| ident(i, strs);

        let expr = |i| expr(i, strs, exprs, nodes);
        do_parse!(
            input,
            tag!("let")
                >> space1
                >> n: ident
                >> space0
                >> tag!("=")
                >> space0
                >> e: expr
                >> (exprs.gc(Expr::Bind(n, e)))
        )
    }

    fn lam<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, GcExpr<'r>> {
        dbg!(input);
        let expr = |i| expr(i, strs, exprs, nodes);

        do_parse!(
            input,
            tag!("\\") >> space0 >> parens >> space0 >> e: expr >> (exprs.gc(Expr::Lam(e)))
        )
    }

    // fn app_<'l, 'r, 'a: 'r, 's: 'r>(
    //     input: &'l str,
    //     strs: &'s Arena<String>,
    //     exprs: &'a Arena<Expr>,
    //     nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    // ) -> GcExpr<'r> {
    //     dbg!(input);
    //     let expr_ = |i| expr(i, strs, exprs, nodes);

    //     let (input, (func, mut args)) = do_parse!(input, e: expr_ >> p: parens >> (e, p)).unwrap();

    //     let mut parms: Map<'r, Ident<'r>, GcExpr<'r>> = Map::default();
    //     loop {
    //         let ident = |i| ident(i, todo!());
    //         let expr_ = |i| expr(i, todo!(), todo!(), nodes);
    //         if args.is_empty() {
    //             break;
    //         }

    //         let (remaining, (p, e)) = do_parse!(
    //             args,
    //             param: ident >> space0 >> tag!(":") >> space0 >> e: expr_ >> (param, e)
    //         )
    //         .unwrap();
    //         parms = parms.insert(p, e, nodes)
    //     }

    //     exprs.gc(Expr::App(func, parms))
    // }

    fn app<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, GcExpr<'r>> {
        dbg!(input);
        // let expr = |i| expr(i, strs, exprs, nodes);
        // let ident = |i| ident(i, strs);
        let expr = |i: &'l str| -> IResult<&'l str, GcExpr<'r>> { todo!() };
        let ident = |i: &'l str| -> IResult<&'l str, Ident<'r>> { todo!() };

        let (input, (func, mut args)) = do_parse!(input, e: expr >> p: parens >> (e, p))?;

        let mut parms: Map<'r, Ident<'r>, GcExpr<'r>> = Map::default();
        loop {
            args = space0(args)?.0;
            if args.is_empty() {
                break;
            }

            // let (remaining, (p, e)) = do_parse!(
            //     args,
            //     param: ident >> space0 >> tag!(":") >> space0 >> e: expr >> (param, e)
            // )?;
            // args = remaining;
            // parms = parms.insert(p, e, nodes)
        }

        Ok((input, exprs.gc(Expr::App(func, parms))))
    }

    // fn app<'l, 'r, 'a: 'r>(
    //     input: &'l str,
    //     strs: &'a Arena<String>,
    //     exprs: &'a Arena<Expr>,
    //     nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    // ) -> IResult<&'l str, GcExpr<'r>> {
    //     dbg!(input);
    //     let expr = |i| expr(i, strs, exprs, nodes);
    //     let ident = |i| ident(i, strs);

    //     let (input, (func, mut args)) = do_parse!(input, e: expr >> p: parens >> (e, p))?;

    //     let mut parms: Map<'r, Ident<'r>, GcExpr<'r>> = Map::default();
    //     loop {
    //         args = space0(args)?.0;
    //         if args.is_empty() {
    //             break;
    //         }

    //         let (remaining, (p, e)) = do_parse!(
    //             args,
    //             param: ident >> space0 >> tag!(":") >> space0 >> e: expr >> (param, e)
    //         )?;
    //         args = remaining;
    //         parms = parms.insert(p, e, nodes)
    //     }

    //     Ok((input, exprs.gc(Expr::App(func, parms))))
    // }

    fn prim<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, GcExpr<'r>> {
        dbg!(input);
        let lit = |i| map!(i, |i| ty(strs, i), |ty| exprs.gc(Expr::Lit(ty)));
        let (input, l) = branch::alt((lit, |i| expr(i, strs, exprs, nodes)))(input)?;
        dbg!(input, l);
        let (input, _) = space0(input)?;

        let (input, op) = if input.starts_with("+") {
            (&input[1..], Op::Add)
        } else if input.starts_with("-") {
            (&input[1..], Op::Sub)
        } else if input.starts_with("*") {
            (&input[1..], Op::Mul)
        } else if input.starts_with("/") {
            (&input[1..], Op::Div)
        } else if input.starts_with("==") {
            (&input[2..], Op::Eql)
        } else {
            return Err(nom::Err::Error(("No PrimOp", ErrorKind::NoneOf)));
        };

        dbg!(input, op);
        let (input, r) = expr(input, strs, exprs, nodes)?;
        dbg!(input, r);

        Ok((input, exprs.gc(Expr::Prim(op, l, r))))
    }

    pub(crate) fn expr<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, GcExpr<'r>> {
        dbg!(input);
        let lit = |i| map!(i, |i| ty(strs, i), |ty| exprs.gc(Expr::Lit(ty)));
        let var = |i| map!(i, |i| ident(i, strs), |n| exprs.gc(Expr::Var(n)));
        let lam = |i| lam(i, strs, exprs, nodes);
        let app = |i| app(i, strs, exprs, nodes);
        let bind = |i| bind(i, strs, exprs, nodes);
        let prim = |i| prim(i, strs, exprs, nodes);

        let (input, _) = space0(input)?;
        dbg!(branch::alt((bind, lam, var, prim, lit, app))(input))
    }

    pub(crate) fn term<'l, 'r, 'a: 'r>(
        input: &'l str,
        strs: &'a Arena<String>,
        exprs: &'a Arena<Expr>,
        nodes: &'a Arena<Node<'static, Ident<'static>, GcExpr<'static>>>,
    ) -> IResult<&'l str, Term<'r>> {
        dbg!(input);
        let term = |i: &'l str| {
            let expr = |i| expr(i, strs, exprs, nodes);
            let ident = |i| ident(i, strs);
            do_parse!(
                i,
                n: ident >> space0 >> tag!("=") >> space0 >> e: expr >> (Term::Fun(n, e))
            )
        };

        let expr = |i: &'l str| {
            let expr = |i| expr(i, strs, exprs, nodes);
            do_parse!(i, e: expr >> (Term::Exp(e)))
        };

        let (input, _) = space0(input)?;
        branch::alt((term, expr))(input)
    }
}

#[derive(Trace, Debug, Copy, Clone, Eq, PartialEq)]
enum Term<'r> {
    Fun(Ident<'r>, GcExpr<'r>),
    Exp(GcExpr<'r>),
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
    Sub,
    Mul,
    Div,
    Eql,
}

impl Op {
    fn eval<'r>(self, a: Value<'r>, b: Value<'r>) -> Value<'r> {
        match (self, a, b) {
            (Op::Add, Type(Int(a)), Type(Int(b))) => Type(Int(a + b)),
            (Op::Sub, Type(Int(a)), Type(Int(b))) => Type(Int(a - b)),
            (Op::Mul, Type(Int(a)), Type(Int(b))) => Type(Int(a * b)),
            (Op::Div, Type(Int(a)), Type(Int(b))) => Type(Int(a / b)),
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
use collections::Node;
use Ty::*;

type Env<'r> = Map<'r, Gc<'r, String>, Value<'r>>;

fn eval<'r, 'a: 'r>(
    env: Env<'r>,
    term: Gc<'r, Expr<'r>>,
    nodes: &'a Arena<Node<Ident<'r>, Value<'r>>>,
) -> Value<'r> {
    match *term {
        Expr::Lit(ty) => Type(ty),
        Expr::Var(n) => *env
            .get(&n)
            .unwrap_or_else(|| panic!("Error: {} is not in scope!", *n)),
        Expr::Lam(a) => Value::Closure(a, env),
        Expr::App(l, args) => match eval(env, l, nodes) {
            Type(ty) => panic!("{:?} is not a function", ty),
            Closure(expr, captured_env) => {
                let args: Map<_, _> = args
                    .iter()
                    .map(|(parm, arg)| (nodes, *parm, eval(env, *arg, nodes)))
                    .collect();
                // Arguments take precedence over variables in scope.
                let env = args.union(captured_env, nodes);

                eval(env, expr, nodes)
            }
        },
        Expr::Prim(op, a, b) => op.eval(eval(env, a, nodes), eval(env, b, nodes)),
        Expr::Bind(name, expr) => {
            eval(env.insert(name, eval(env, expr, nodes), nodes), term, nodes)
        }
    }
}
